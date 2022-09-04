(ns com.xadecimal.procedural
  (:require [clojure.walk :as walk]
            [proteus :as prot])
  (:import [com.xadecimal.procedural ExBreak ExContinue]))

(load "procedural/common")
(load "procedural/loop")

(defn- find-and-replace-vars-mark-skip
  [body]
  (let [vars (atom #{})
        forms (prewalk
               (fn[form]
                 (cond (and (list? form) (= 'var (first form)) (= (count form) 3))
                       (let [var-sym (second form)
                             var-val (nth form 2)]
                         (swap! vars conj var-sym)
                         (reduced (list ::skip `(vreset! ~var-sym ~var-val))))
                       (and (symbol? form) (contains? @vars form))
                       (reduced (list ::skip `(deref ~form)))
                       :else
                       form))
               body)]
    {:vars @vars
     :forms forms}))

(defn- replace-tdz
  [forms vars]
  (prewalk
   (fn[form]
     (cond (and (list? form) (= ::skip (first form)))
           (reduced (second form))
           (and (symbol? form) (contains? vars form))
           (reduced `(when-some [v# (deref ~form)]
                       (if (= ~::reference-error (:type (ex-data v#)))
                         (throw v#)
                         v#)))
           :else
           form))
   forms))

(defmacro var-scope
  [& body]
  (let [{:keys [vars forms]} (find-and-replace-vars-mark-skip body)
        forms (replace-tdz forms vars)]
    `(let ~(vec
            (mapcat
             (fn[var]
               [var
                `(volatile!
                  (ex-info
                   (str "ReferenceError: Cannot access '" '~var "' before initialization")
                   {:type `::reference-error
                    :var '~var}))])
             vars))
       ~@forms)))

((prot/let-mutable
  [i 1
   j nil]
  (let [f ^:local #(println i)] ;; TODO: Deal with this issue!
    (set! i 2)
    (when (= 1 1)
      (set! i 3))
    (set! j 4)
    (println i j)
    f)))

((var-scope
  (var i 1)
  (let [f #(println i)] ;; TODO: Deal with this issue!
    (var i 2)
    (when (= 1 1)
      (var i 3))
    (var j 4)
    (println i j)
    f)))

#_(let [i 10]
    (let [x 50]
      (var i x))
    i)
