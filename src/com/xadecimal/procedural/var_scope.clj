(ns com.xadecimal.procedural.var-scope
  (:require
   [com.xadecimal.riddley.walk :as riddley]))

(defn- get-local-binding-info
  "Returns the local-binding's Class/primitive info if it has any.

   Example return: {:class long :primitive? true}"
  [local-binding]
  (try
    (when-some [java-class (.getJavaClass local-binding)]
      {:class java-class
       :primitive? (.isPrimitive java-class)})
    (catch Exception _e)))

(defn- get-local-binding-type
  [local-binding]
  (or
   ;; This is used for outer var-scope var, the var-scope
   ;; macro also adds a ::tag meta to it's local symbols,
   ;; so that inner var-scopes can refer to the type of
   ;; the value inside the array, otherwise the type found
   ;; would be an array of type, instead of type.
   (::tag (meta (.-sym local-binding)))
   ;; This covers the rest, values wrapped in primitive casts
   ;; or functions with their arg vector type hinted
   ;; as well as all literals and type hints on the local.
   (:class (get-local-binding-info local-binding))
   Object))

(defn object-array2
  "Creates an array of objects of size 1 with value as init-value.
   We use this because object-array differs in signature to all other
   typed array constructors, so we provide an object-array2 that follows
   the same pattern of [size init-val]"
  {:inline (fn [_size init-value]
             `(. clojure.lang.RT ~'object_array ~[init-value]))}
  [_size init-value]
  (. clojure.lang.RT object_array [init-value]))

(defn- ->typed-array-constructor
  [array-type]
  (condp some #{array-type}
    #{'boolean Boolean Boolean/TYPE} `boolean-array
    #{'byte Byte Byte/TYPE} `byte-array
    #{'char Character Character/TYPE} `char-array
    #{'short Short Short/TYPE} `short-array
    #{'int Integer Integer/TYPE} `int-array
    #{'long Long Long/TYPE} `long-array
    #{'float Float Float/TYPE} `float-array
    #{'double Double Double/TYPE} `double-array
    `object-array2))

(defmacro var-initialization
  [var-sym & body]
  (let [type (-> (get &env var-sym) (get-local-binding-type))]
    `(let [~(vary-meta var-sym assoc ::tag type)
           (~(->typed-array-constructor type) 1 ~var-sym)]
       ~@body)))

(defn- var-initialization-form?
  [form]
  (and (seq? form)
       (= 'var (nth form 0))
       (symbol? (nth form 1))
       (not= ::not-found (nth form 2 ::not-found))))

(defn- ->var-initialization
  [body form]
  (let [var-sym (nth form 1)
        var-val (nth form 2)]
    `(let [~var-sym ~var-val]
       (var-initialization
        ~var-sym
        (var-assignment-and-access ~@body)))))

(defn- var-assignment-form?
  [form seen-vars]
  (and (seq? form)
       (= 'set! (nth form 0))
       (symbol? (nth form 1))
       (contains? seen-vars (nth form 1))
       (not= ::not-found (nth form 2 ::not-found))))

(defn- ->var-assignment
  [form]
  (let [var-sym (nth form 1)
        var-value (nth form 2)]
    `(aset ~var-sym 0 ~var-value)))

(defn- var-access-form?
  [form seen-vars]
  (and (symbol? form)
       (contains? seen-vars form)))

(defn- ->var-access
  [form]
  `(aget ~form 0))

(defn- var-scope-form?
  [form]
  (and (seq? form)
       (#{'var-scope
          `var-scope} (nth form 0))))

(defn- var-assignment-and-access-form?
  [form]
  (and (seq? form)
       (#{'var-assignment-and-access
          `var-assignment-and-access} (nth form 0))))

(defn- var-initialization-macro-form?
  [form]
  (and (seq? form)
       (#{'var-initialization
          `var-initialization} (nth form 0))))

(defn- add-var-assignment-and-access
  [body env-vars]
  (let [vars
        ;; Start with all local vars whose meta is tagged with ::tag which
        ;; indicates it's a local var that was created by var-scope
        ;; as we only want to touch those locals.
        (into #{} (filter #(::tag (meta %))) env-vars)]
    (riddley/walk-exprs
     (fn predicate [form]
       (or (var-assignment-form? form vars)
           (var-access-form? form vars)
           (var-assignment-and-access-form? form)
           (var-scope-form? form)
           (var-initialization-macro-form? form)))
     (fn handler [form]
       (cond (var-assignment-form? form vars)
             (->var-assignment form)
             (var-access-form? form vars)
             (->var-access form)
             :else
             form))
     #{'set! 'var-assignment-and-access
       `var-assignment-and-access 'var-scope
       `var-scope 'var-initialization
       `var-initialization}
     body)))

(defmacro var-assignment-and-access
  [& body]
  `(do
     ~@(add-var-assignment-and-access
        body
        ;; Get the symbol of every locals.
        ;; You can't use the key of &env, because that is not
        ;; the correct symbol in the case of shadowing, you have
        ;; to grab the symbol from the LocalBinding on the value of &env
        ;; or the meta will be wrong.
        (map (fn[[_k v]] (.-sym v)) &env))))

(defn- add-var-scope
  "Takes body, and for every (var sym value) forms, which we call
   var-initialization forms, it'll nest all forms that come after it
   inside our ->var-initialization generating form.

   '((println 100)
     (var i 10)
     (+ i i))

   becomes:

   (var-assignment-and-access
     (println 100)
     (let [i 10]
       (var-initialization i
         (var-assignment-and-access (+ i i)))))"
  [body]
  (let [stack (atom '())
        nested-form (atom '())
        seen-vars (atom #{})]
    ;; Push all top-level form to stack
    (doseq [form body]
      (when (var-initialization-form? form)
        (let [[_ var-sym] form]
          (when (contains? @seen-vars var-sym)
            (throw (ex-info
                    (str "Variable " var-sym " is already defined in current scope. You're not allowed to define the same var twice in the same scope.")
                    {:ex-type :invalid-input
                     :input form})))
          (swap! seen-vars conj var-sym)))
      (swap! stack conj form))
    ;; While stack isn't empty
    (while (seq @stack)
      ;; Pop form from stack
      (let [form (ffirst (swap-vals! stack pop))]
        ;; When the form isn't a (var sym value)
        (if (not (var-initialization-form? form))
          ;; Add it to our new nested form we're creating
          (swap! nested-form #(cons form %))
          ;; And once the form is a (var sym value)
          ;; our nested-form is complete, we finish it
          ;; by making it a let form with our var as an
          ;; array initialized to our value inside an array
          (do
            (swap! nested-form ->var-initialization form)
            ;; We add it back to the stack because it might need
            ;; to be nested itself in another form
            (swap! stack conj @nested-form)
            ;; We create a new nested-form where we'll nest the rest
            (reset! nested-form '())))))
    `(var-assignment-and-access ~@@nested-form)))

(defmacro var-scope
  "Creates a block scope where you can declare mutable variables using
   (var <name> <value>) and mutate it using (set! <name> <new-value>).

   Mutable variables follow these semantics:
    - variables are scoped to their block
    - variables are allowed to shadow each other
    - inner block scope can reuse the name of an outer block scope variable
    - you cannot refer to a variable declared later prior to it being declared
    - you cannot redefine the same variable (var i 0) (var i 1), you must
      instead assign a different value to existing defined variable: (set! i 1)
    - when var is primitive, you cannot change the inferred type through
      mutation, if defined as a long only long values can be set! to it
    - inner block scopes see all variables from outer blocks and can access and
      mutate them"
  [& body]
  (add-var-scope body))
