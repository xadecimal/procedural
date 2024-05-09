(ns com.xadecimal.procedural.procedure-impl
  (:require [com.xadecimal.mutable-var :refer [var-scope]]
            [clojure.spec.alpha :as s]
            [clojure.core.specs.alpha :as specs])
  (:import [com.xadecimal.procedural ExReturn]))

;;; Used to implement defn and fn extension macros that support all what fn
;;; and defn do, but also wraps it all in a try/catch for (return) support and
;;; in a var-scope for local mutable variable support.

;; From https://blog.klipse.tech/clojure/2019/03/08/spec-custom-defn.html
(s/def ::specs/seq-binding-form
  (s/and vector?
         (s/conformer identity vec)
         (s/cat :elems (s/* ::specs/binding-form)
                :rest (s/? (s/cat :amp #{'&} :form ::specs/binding-form))
                :as (s/? (s/cat :as #{:as} :sym ::specs/local-name)))))

(defn arg-list-unformer [a]
  (vec
   (if (and (coll? (last a)) (= '& (first (last a))))
     (concat (drop-last a) (last a))
     a)))

(s/def ::specs/param-list
  (s/and
   vector?
   (s/conformer identity arg-list-unformer)
   (s/cat :args (s/* ::specs/binding-form)
          :varargs (s/? (s/cat :amp #{'&} :form ::specs/binding-form)))))

(s/def ::fn-args
  (s/cat :fn-name (s/? simple-symbol?)
         :fn-tail (s/alt :arity-1 ::specs/params+body
                         :arity-n (s/+ (s/spec ::specs/params+body)))))

(defn conform-defn
  [args]
  (s/conform ::specs/defn-args args))

(defn unform-defn
  [args]
  (s/unform ::specs/defn-args args))

(defn conform-fn
  [args]
  (s/conform ::fn-args args))

(defn unform-fn
  [args]
  (s/unform ::fn-args args))

(defn update-conf
  [{[arity] :fn-tail :as conf} body-update-fn]
  (case arity
    :arity-1 (update-in
              conf
              [:fn-tail 1 :body 1]
              body-update-fn)
    :arity-n (update-in
              conf
              [:fn-tail 1 :bodies]
              (fn [bodies]
                (prn bodies)
                (map
                 (fn [body] (update-in body [:body 1] body-update-fn))
                 bodies)))))

(defn wrap-var-scope
  [body]
  `((try
      (var-scope ~@body)
      (catch ExReturn t#
        (case (.-type t#)
          0 (.-o t#)
          1 (.-l t#)
          2 (.-d t#)
          3 (.-b t#)
          4 nil)))))
