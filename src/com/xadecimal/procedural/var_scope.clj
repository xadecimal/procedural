(ns com.xadecimal.procedural.var-scope
  (:require
   [com.xadecimal.procedural.common :as cm])
  (:import
   [clojure.lang Compiler$CompilerException]))

(defn- object-array2
  "Creates an array of objects of size 1 with value as init-value.
   We use this because object-array differs in signature to all other
   typed array constructors, so we provide an object-array2 that follows
   the same pattern of [size init-val]"
  {:inline (fn [_size init-value]
             `(. clojure.lang.RT ~'object_array ~[init-value]))}
  ([_size init-value] (. clojure.lang.RT object_array [init-value])))

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

(defn- ->array-type
  [expr types-map]
  (try
    (or (get types-map expr)
        (:tag (meta expr))
        (:class (cm/expression-info expr))
        Object)
    (catch Compiler$CompilerException _
      Object)))

(defn- var-initialization-form?
  [form]
  (and (seq? form)
       (= 'var (nth form 0))
       (symbol? (nth form 1))
       (not= ::not-found (nth form 2 ::not-found))))

(defn- ->var-initialization
  [body form seen-vars]
  (let [var-sym (nth form 1)
        var-val (nth form 2)
        type (get seen-vars var-sym)]
    `(let [~(vary-meta var-sym assoc ::tag type)
           (~(->typed-array-constructor type) 1 ~var-val)]
       ~@body)))

(defn- var-assignment-form?
  [form seen-vars]
  (and (seq? form)
       (= 'set! (nth form 0))
       (symbol? (nth form 1))
       (get seen-vars (nth form 1))
       (not= ::not-found (nth form 2 ::not-found))))

(defn- ->var-assignment
  [form]
  (let [var-sym (nth form 1)
        var-value (nth form 2)]
    `(aset ~var-sym 0 ~var-value)))

(defn- var-access-form?
  [form seen-vars]
  (and (symbol? form)
       (get seen-vars form)))

(defn- ->var-access
  [form]
  `(aget ~form 0))

(defn- var-scope-form?
  [form]
  (and (seq? form)
       (= 'var-scope (nth form 0))))

(defn- add-var-scope
  [body outer-vars]
  (let [stack (atom '())
        nested-form (atom '())
        ;; Map of sym -> type, which tracks the vars we've seen
        ;; along with their inferred type.
        seen-vars (atom outer-vars)]
    ;; Push all top-level form to stack
    (doseq [form body]
      ;; Track the vars we see as we push them and analyze their type, if
      ;; a var has a prior seen one as it's value we will use the type of
      ;; the previous one as its type.
      (when (var-initialization-form? form)
        (swap! seen-vars assoc (nth form 1) (->array-type (nth form 2) @seen-vars)))
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
            (swap! nested-form ->var-initialization form @seen-vars)
            ;; We add it back to the stack because it might need
            ;; to be nested itself in another form
            (swap! stack conj @nested-form)
            ;; We create a new nested-form where we'll nest the rest
            (reset! nested-form '())))))
    `(do ~@@nested-form)))

(defn caca
  ^long [xs]
  (reduce + xs))

(def ^{:tag 'long} k 100)

#_(let [i (long k)]
    (var-scope
     (println 100)
     (var i i)
     (println :i i)
     (var j i)
     (println :j j)
     (var-scope
      (println :j j)
      (var i 200)
      (var j j)
      (println :i i)
      (println :j j))))

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
  (let [env &env
        ;; Find the type of local vars and pass them to add-var-scope so that
        ;; if we are initializing a var with the value of a reference to another
        ;; local it will have the type of the referred local.
        ;; i.e: (let [i 10] (var j i)) -> we want j to be ^long same as i
        env-vars (into {}
                       (map (fn[[k v]]
                              [k (or
                                  ;; This is used for outer var-scope var, the var-scope
                                  ;; macro also adds a ::tag meta to it's local symbols,
                                  ;; so that inner var-scopes can refer to the type of
                                  ;; the value inside the array, otherwise the type found
                                  ;; would be an array of type, instead of type.
                                  (::tag (meta (.-sym v)))
                                  ;; tag seems needed when the local itself refers to
                                  ;; a global var and we type-hint the local sym
                                  (.-tag v)
                                  ;; This covers the rest, values wrapped in primitive casts
                                  ;; or functions with their arg vector type hinted
                                  ;; as well as all literals
                                  (:class (cm/get-compiler-class-info v)))]))
                       env)]
    (add-var-scope body env-vars)))
