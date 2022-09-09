(ns com.xadecimal.procedural
  (:require [clojure.walk :as walk]
            [clojure.set :as set]
            [criterium.core :as crit]
            [com.xadecimal.riddley.walk :as rw :refer [walk-exprs]]
            [com.xadecimal.riddley.compiler :refer [locals]]
            [clojure.set :as set])
  (:import [com.xadecimal.procedural ExBreak ExContinue]
           [clojure.lang Compiler Compiler$C Compiler$FnExpr
            Compiler$BodyExpr Compiler$FnMethod
            Compiler$CompilerException RT]))

(load "procedural/common")
(load "procedural/var_scope")
(load "procedural/loop")

#_((var-scope
    (var i nil)
    (let [f #(println i)] ;; TODO: Deal with this issue!
      (set! i 2)
      (when (= 1 1)
        (set! i 3))
      (var j 4)
      (println i j)
      f)))

#_(var-scope
   (var i 1)
   (println i)
   (let [i 2]
     (println i))
   (let [i 3]
     (println i))
   (println i))

(defn aa []
  (let [i (long-array 1)]
    (aset i 0 1)
    (println (aget i 0))
    (if (= 1 (aget i 0))
      (aset i 0 2))
    (println (aget i 0))
    (let [i (long-array 1)]
      (aset i 0 100)
      (println (aget i 0))
      (if (> (aget i 0) 50)
        (aset i 0 10000))
      (println (aget i 0)))
    (println (aget i 0))))

#_(aa)

#_(var-scope
   (var i 1)
   (println i)
   (if (= 1 i)
     (set! i 2))
   (println i)
   (var-scope
    (var i 0)
    ((fn[] (set! i 100)))
    (var j "hello")
    (println i)
    (println j)
    (if (> i 50)
      (set! i 10000))
    (println i)
    (set! j "pipi")
    (println j))
   (println i))

(def w (fn[] (reduce + [1 2 3 4])))
#_(var-scope
   (var i ^long (w))
   (var j 0)
   (set! j i)
   (var g j)
   (+ i j g))

#_(var-scope
   (fn[] i)
   (var i 0))
