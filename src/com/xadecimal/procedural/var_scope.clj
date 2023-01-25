(ns com.xadecimal.procedural.var-scope
  (:require
   [com.xadecimal.procedural.common :as cm]
   [com.xadecimal.riddley.walk :as rw])
  (:import
   [clojure.lang Compiler$CompilerException]))

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
    `object-array))

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
  [form]
  (let [var-sym (nth form 1)
        var-value (nth form 2)]
    `(aset ~var-sym 0 ~var-value)))

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
        seen-here (atom {})
        var-defined-body (rw/walk-exprs
                          (fn predicate [_form]
                            (if @unwrapped?
                              true
                              (do (reset! unwrapped? true)
                                  false)))
                          (fn handler [form]
                            (cond (var-initialization-form? form) ;; (var i 10) -> (aset i 10)
                                  (let [var-sym (nth form 1)
                                        var-type (->array-type (nth form 2) @seen)]
                                    (swap! seen assoc var-sym var-type)
                                    (swap! seen-here assoc var-sym var-type)
                                    (->var-initialization form))
                                  (var-scope-form? form)
                                  (add-var-scope (rest form) @seen)
                                  :else
                                  (add-var-scope-inside form @seen)))
                          '#{var var-scope}
                          body)]
    `(let ~(reduce-kv
            (fn[acc k v]
              (-> acc
                  (conj k)
                  (conj `(~(->typed-array-constructor v) 1))))
            []
            @seen-here)
       ~@var-defined-body)))

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
