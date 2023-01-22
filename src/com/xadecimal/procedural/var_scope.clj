(ns com.xadecimal.procedural.var-scope
  (:require [com.xadecimal.procedural.common :refer [conj-distinctv expression-info]]
            [com.xadecimal.riddley.compiler :refer [locals]]
            [com.xadecimal.riddley.walk :as rw :refer [walk-exprs]])
  (:import [clojure.lang Compiler$CompilerException]))

(defn- find-and-replace-vars
  ([body] (find-and-replace-vars body (atom {}) (atom [])))
  ([body vars ordered-vars]
   (let [forms (walk-exprs
                (fn[form]
                  (or (and (seq? form) (= 'var (first form))
                           (not (contains? @vars (second form))) (= (count form) 3))
                      (and (seq? form) (= 'set! (first form))
                           (contains? @vars (second form)) (= (count form) 3))
                      (and (seq? form)
                           (or (and (contains? #{'aset `aset 'aget `aget} (first form))
                                    (contains? @vars (second form)))
                               (and (= `[. RT] (take 2 form))
                                    (seq? (nth form 2))
                                    (contains? #{'aset `aset 'aget `aget} (first (nth form 2)))
                                    (contains? @vars (second (nth form 2))))))
                      (and (contains? @vars form) (not (contains? (locals) form)))))
                (fn[form]
                  (cond (and (seq? form) (= 'var (first form))
                             (not (contains? @vars (second form))) (= (count form) 3))
                        (let [var-sym (second form)
                              var-val (nth form 2)
                              _ (swap! ordered-vars conj-distinctv var-sym)
                              _ (swap! vars update var-sym conj-distinctv var-val)
                              deref-var-val (:forms (find-and-replace-vars var-val vars ordered-vars))]
                          `(aset ~var-sym 0 ~deref-var-val))
                        (and (seq? form) (= 'set! (first form))
                             (contains? @vars (second form)) (= (count form) 3))
                        (let [var-sym (second form)
                              var-val (nth form 2)
                              _ (swap! vars update var-sym conj-distinctv var-val)
                              deref-var-val (:forms (find-and-replace-vars var-val vars ordered-vars))]
                          `(aset ~var-sym 0 ~deref-var-val))
                        (and (seq? form)
                             (or (and (contains? #{'aset `aset 'aget `aget} (first form))
                                      (contains? @vars (second form)))
                                 (and (= `[. RT] (take 2 form))
                                      (seq? (nth form 2))
                                      (contains? #{'aset `aset 'aget `aget} (first (nth form 2)))
                                      (contains? @vars (second (nth form 2))))))
                        form
                        (and (contains? @vars form) (not (contains? (locals) form)))
                        `(aget ~form 0)))
                '#{var}
                body)]
     {:vars @vars
      :ordered-vars @ordered-vars
      :forms forms})))

(defn- assert-vars-not-out-of-scope
  [body vars]
  (let [seen-vars (atom #{})]
    (walk-exprs
     (fn[form]
       (when (and (seq? form) (= 'var (first form)) (= (count form) 3))
         (let [var-sym (second form)]
           (swap! seen-vars conj var-sym)))
       (and (contains? vars form) (not (contains? (set (keys (locals))) form))))
     (fn[form]
       (when (not (contains? @seen-vars form))
         (throw (ex-info (str "Uninitialized var: " form) {}))))
     '#{var}
     body)))

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
  [var-vals types-map]
  (let [types (reduce
               (fn[types var-val]
                 (conj types
                       (try
                         (or
                          (get types-map var-val)
                          (:class (expression-info var-val))
                          Object)
                         (catch Compiler$CompilerException _
                           Object))))
               []
               var-vals)]
    (if (apply = types)
      (first types)
      Object)))

(defn- add-var-scope
  [body]
  (let [{:keys [ordered-vars vars forms]} (find-and-replace-vars body)]
    (assert-vars-not-out-of-scope body vars)
    `(let
         ~(vec
           (mapcat
            (let [previous-types (atom {})]
              (fn[var-sym]
                (let [var-vals (get vars var-sym)]
                  [var-sym `(~(let [type (->array-type var-vals @previous-types)]
                                (swap! previous-types assoc var-sym type)
                                (->typed-array-constructor type))
                             1)])))
            ordered-vars))
       ~@forms)))

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
