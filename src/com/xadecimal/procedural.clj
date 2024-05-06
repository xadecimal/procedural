(ns com.xadecimal.procedural
  (:require [com.xadecimal.procedural.loop-impl :as loop-impl]
            [com.xadecimal.procedural.procedure-impl :as proc-impl]
            [com.xadecimal.mutable-var :refer [var-scope]])
  (:import [com.xadecimal.procedural ExReturn]))


;;;;;;;;;;;;;;;
;;;; Blocks

(defmacro do!
  [& body]
  `(var-scope ~@body))

;;;; Blocks
;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;;;; Loops

(defn break
  "Breaks out of for-loop. Can be called inside a function call as long
   as its within the scope and extent of the for-loop."
  {:inline (fn [] `(throw loop-impl/ex-break))}
  []
  (throw loop-impl/ex-break))

(defn continue
  "Continues to next iteration skipping over what's left in current pass.
   Can be called inside a function call as long as its within the scope
   and extent of the for-loop."
  {:inline (fn [] `(throw loop-impl/ex-continue))}
  []
  (throw loop-impl/ex-continue))

(defmacro for!
  {:style/indent 3}
  [init condition mutation & body]
  (loop-impl/for-loop init condition mutation body))

(defmacro while!
  {:style/indent 1}
  [condition & body]
  (loop-impl/while-loop condition body))

(defmacro do-while!
  {:style/indent 1}
  [condition & body]
  (loop-impl/do-while-loop condition body))

;;;; Loops
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;;;; Assignments

(defmacro !=
  [v e]
  `(set! ~v ~e))

(defmacro +=
  [v e]
  `(set! ~v (+ ~v ~e)))

(defmacro -=
  [v e]
  `(set! ~v (- ~v ~e)))

(defmacro *=
  [v e]
  `(set! ~v (* ~v ~e)))

(defmacro div=
  [v e]
  `(set! ~v (/ ~v ~e)))

(defmacro rem=
  [v e]
  `(set! ~v (rem ~v ~e)))

(defmacro bitand=
  [v e]
  `(set! ~v (bit-and ~v ~e)))

(defmacro bitxor=
  [v e]
  `(set! ~v (bit-xor ~v ~e)))

(defmacro bitor=
  [v e]
  `(set! ~v (bit-or ~v ~e)))

(defmacro bitleft=
  [v e]
  `(set! ~v (bit-shift-left ~v ~e)))

(defmacro bitright=
  [v e]
  `(set! ~v (bit-shift-right ~v ~e)))

(defmacro unbitright=
  [v e]
  `(set! ~v (unsigned-bit-shift-right ~v ~e)))

;;;; Assignments
;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;;;; Unary

(defmacro ++
  [v]
  `(set! ~v (inc ~v)))

(defmacro --
  [v]
  `(set! ~v (dec ~v)))

;;;; Unary
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;
;;;; Conditionals

(defmacro when!
  [test & body]
  `(when ~test (var-scope ~@body)))

(defmacro when-not!
  [test & body]
  `(when-not ~test (var-scope ~@body)))

;;;; Conditionals
;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;
;;;; Procedures

(defn return
  {:inline (fn
             ([]
              `(throw (ExReturn.)))
             ([v]
              `(throw (ExReturn. ~v))))}
  ([]
   (throw (ExReturn.)))
  ([v]
   (throw (ExReturn. v))))

(defmacro defn!
  [& args]
  (let [conf (proc-impl/conform-defn args)
        new-conf (proc-impl/update-conf conf (partial proc-impl/wrap-var-scope))
        new-args (proc-impl/unform-defn new-conf)]
    (cons `defn new-args)))

(defmacro fn!
  [& args]
  (let [conf (proc-impl/conform-fn args)
        new-conf (proc-impl/update-conf conf (partial proc-impl/wrap-var-scope))
        new-args (proc-impl/unform-fn new-conf)]
    (cons `fn new-args)))

;;;; Procedures
;;;;;;;;;;;;;;;;;;;
