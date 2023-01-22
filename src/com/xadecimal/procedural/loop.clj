(ns com.xadecimal.procedural.loop
  (:require [com.xadecimal.procedural.common :refer [ex-break ex-continue]]
            [com.xadecimal.riddley.walk :as rw :refer [walk-exprs]])
  (:import [com.xadecimal.procedural ExBreak ExContinue]))

(defn- insert-throws
  [body]
  (walk-exprs
   (fn[form]
     (or (and (seq? form) (= 'break (first form)))
         (and (seq? form) (= 'continue (first form)))))
   (fn[form]
     (cond (and (seq? form) (= 'break (first form)))
           `(throw ex-break)
           (and (seq? form) (= 'continue (first form)))
           `(throw ex-continue)))
   body))

#_(for-loop [i 0 (< i 10) (inc i)]
    (if (= 4 i)
      (continue)
      (println i)))

#_(for-loop [i 0 (< i 10) (+ i 3)]
    (if (= 4 i)
      (continue)
      (for-loop [y i (< y 10) (inc y)]
        (println i y)))
    (println i))

#_(for-loop [[i 0 j 10] (and (< i 10) (> j 0)) [(inc i) (dec j)]]
    (if (= 4 i)
      (continue)
      (println i j)))

#_(try
    (var-scope
     (var i 0)
     (while (< i 10)
       (try
         (if (= 4 i)
           (continue)
           (println i))
         (catch ExContinue t#)
         (finally (set! i (inc i))))))
    (catch ExBreak t#))

(defn- for-to-loop
  "Rewrite body so it is wrapped in a try/catch and var-scope that simulates an
   imperative for-loop. It will define a mutable var for the pair s i or for each
   pair inside the vector s. Then it will start a loop checking for condition c
   (or i), and at the end of the loop will perform p or each action in c if a
   vector. It'll also try/catch throws of break and continue exceptions to
   simulate imperative break and continue behavior. You can use (break) to break,
   and (continue) to continue, for-to-loop will replace them by (throw ex-break)
   and (throw ex-continue) accordingly."
  [[s i c p] body]
  `(try
     (var-scope
      ~@(if (vector? s)
          (->> (partition 2 s)
               (map (fn[[s i]] `(var ~s ~i))))
          `((var ~s ~i)))
      (while ~(if (vector? s) i c)
        (try
          ~@(insert-throws body)
          (catch ExContinue t#)
          (finally ~@(if (vector? s)
                       (map (fn[s p] `(set! ~s ~p))
                            (map first (partition 2 s)) c)
                       `((set! ~s ~p)))))))
     (catch ExBreak t#)))

(defn break
  "Breaks out of for-loop. Can be called inside a function call as long
   as its within the scope and extent of the for-loop."
  []
  (throw ex-break))

(defn continue
  "Continues to next iteration skipping over what's left in current pass.
   Can be called inside a function call as long as its within the scope
   and extent of the for-loop."
  []
  (throw ex-continue))

(defmacro for-loop
  "Imperative for-loop, always returns nil, assumes side-effects.

   First argument is a vector of [declaration condition mutation]
     (var i = 0; i < 10; i++) -> [i 0 (< i 10) (inc i)]
   Can also take multiple declaration by wrapping in extra vector
     (var i = 0, var j = 10; i < 10 && j > 0; i++, j--)
     -> [[i 0 j 10] (and (< i 10) (> j 0)) [(inc i) (dec j)]]

   Rest is body of loop.

   Just like in an imperative for-loop, you can break out of the loop early by
   calling the `(break)` function, or skip to the next iteration by calling the
   `(continue)` function from anywhere inside the body, or inside a function
   called by the body of the loop.

   Example:
     (for-loop [[i 0 j 10] (and (< i 10) (> j 0)) [(inc i) (dec j)]]
       (when (= 3 i) (continue))
       (when (= 5 i) (break))
       (println i j))"
  {:style/indent 1}
  [loop-vector & body]
  (for-to-loop loop-vector body))

(defmacro while-loop
  "Imperative while-loop, always returns nil, assumes side-effects.

   First argument is test condition, will loop until test condition
   evaluates to false.

   Rest is body of loop.

   Just like in an imperative while-loop, you can break out of the loop
   early by calling the `(break)` function, or skip to the next iteration
   by calling the `(continue)` function from anywhere inside the body, or
   inside a function called by the body of the loop.

   Example:
     (def i (atom 0))
     (while-loop (< @i 10)
       (when (= @i 4) (swap! i inc) (continue))
       (println @i)
       (swap! i inc)
       (when (= @i 8) (break)))"
  {:style/indent 1}
  [test & body]
  (for-to-loop [[] test []] body))
