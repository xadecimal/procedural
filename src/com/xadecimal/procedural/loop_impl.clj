(ns com.xadecimal.procedural.loop-impl
  (:require [com.xadecimal.mutable-var :refer [var-scope]])
  (:import [com.xadecimal.procedural ExBreak ExContinue]))

(def ex-break (ExBreak.))
(def ex-continue (ExContinue.))

(defn for-loop
  "Rewrite body so it is wrapped in a try/catch and var-scope that simulates an
   imperative for-loop. It will first evaluate init statement, assuming mutable
   var initialization, if wrapped in a vector will initialize each form in the
   vector one by one. Then it will start a loop checking for condition, and at
   the end of the loop will perform mutation, assuming side-effect to the
   initialized vars, if wrapped in a vector will perform each mutation form in
   the vector one by one. It'll also try/catch throws of break and continue
   exceptions to simulate imperative break and continue behavior. You can use
   (break) to break, and (continue) to continue, for-loop will replace them by
   (throw ex-break) and (throw ex-continue) accordingly."
  [init condition mutation body]
  `(try
     (var-scope
       ~@(if (vector? init)
           init
           `(~init))
       (while ~condition
         (try
           (var-scope
             ~@body)
           (catch ExContinue t#)
           (finally ~@(if (vector? mutation)
                        mutation
                        `(~mutation))))))
     (catch ExBreak t#)))

(defn while-loop
  "Rewrite body so it is wrapped in a try/catch and var-scope that simulates an
   imperative while-loop. Will evaluate body until condition is false, assumes
   some side-effect will change condition to false. It'll also try/catch throws
   of break and continue exceptions to simulate imperative break and continue
   behavior. You can use (break) to break, and (continue) to continue,
   do-while-loop will replace them by (throw ex-break) and (throw ex-continue)
   accordingly."
  [condition body]
  `(try
     (loop []
       (when ~condition
         (try
           (var-scope
             ~@body)
           (catch ExContinue t#))
         (recur)))
     (catch ExBreak t#)))

(defn do-while-loop
  "Rewrite body so it is wrapped in a try/catch and var-scope that simulates an
   imperative do-while-loop. Will evaluate body once, and until condition is
   false thereafter, assumes some side-effect will change condition to false.
   It'll also try/catch throws of break and continue exceptions to simulate
   imperative break and continue behavior. You can use (break) to break, and
   (continue) to continue, do-while-loop will replace them by (throw ex-break) and
   (throw ex-continue) accordingly."
  [condition body]
  `(try
     (loop []
       (try
         (var-scope
           ~@body)
         (catch ExContinue t#))
       (when ~condition
         (recur)))
     (catch ExBreak t#)))
