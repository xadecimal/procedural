(ns com.xadecimal.procedural.var-scope
  (:require
   [com.xadecimal.procedural.common :as cm]
   [com.xadecimal.riddley.walk :as rw])
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
    #{Boolean Boolean/TYPE} `boolean-array
    #{Byte Byte/TYPE} `byte-array
    #{Character Character/TYPE} `char-array
    #{Short Short/TYPE} `short-array
    #{Integer Integer/TYPE} `int-array
    #{Long Long/TYPE} `long-array
    #{Float Float/TYPE} `float-array
    #{Double Double/TYPE} `double-array
    `object-array2))

(defn- ->array-type
  [expr types-map]
  (try
    (or (get types-map expr)
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
  [body form seen]
  (->> body
       (cons [(nth form 1)
              `(~(->typed-array-constructor
                  (->array-type (nth form 2) @seen))
                1
                ~(nth form 2))])
       (cons `let)))

(defn- nest-on-var-definition
  [body seen]
  (let [stack (atom '())
        new-list (atom '())
        input body]
    (doseq [e input] (swap! stack conj e))
    (while (seq @stack)
      (let [form (ffirst (swap-vals! stack pop))]
        (if (not (var-initialization-form? form))
          (swap! new-list #(cons form %))
          (do
            (swap! seen assoc (nth form 1) (->array-type (nth form 2) @seen))
            (swap! new-list ->var-initialization form seen)
            (swap! stack conj @new-list)
            (reset! new-list '())))))
    @new-list))

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

(declare add-var-scope)

(defn- add-var-scope-inside
  [body seen-vars]
  (rw/walk-exprs
   (fn predicate [form]
     (or (var-assignment-form? form seen-vars)
         (var-access-form? form seen-vars)
         (var-scope-form? form)))
   (fn handler [form]
     (cond (var-assignment-form? form seen-vars) ;; (set! i 20) -> (aset i 0 20)
           (->var-assignment form)
           (var-access-form? form seen-vars)
           (->var-access form)
           (var-scope-form? form)
           (add-var-scope (rest form) seen-vars)))
   '#{var var-scope}
   body))

(defn- add-var-scope
  [body init-seen]
  (let [unwrapped? (atom false)
        seen (atom init-seen)
        body (nest-on-var-definition body seen)]
    `(do
       ~@(rw/walk-exprs
          (fn predicate [_form]
            (if @unwrapped?
              true
              (do (reset! unwrapped? true)
                  false)))
          (fn handler [form]
            (cond (var-scope-form? form)
                  (add-var-scope (rest form) @seen)
                  :else
                  (add-var-scope-inside form @seen)))
          '#{var var-scope}
          body))))

#_(var-scope
   (println 100)
   (var i 10)
   (println i))

#_(do (println 100)
      (let [i (long-array 1 10)]
        (println (aget i 0))))

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
  (add-var-scope body {}))
