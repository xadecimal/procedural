(ns com.xadecimal.procedural
  (:require [com.xadecimal.procedural.loop-impl :as loop-impl]
            [com.xadecimal.procedural.procedure-impl :as proc-impl]
            [com.xadecimal.mutable-var :refer [var-scope]]
            [clojure.spec.alpha :as s])
  (:import [com.xadecimal.procedural ExReturn]))


;;;;;;;;;;;;;;;
;;;; Blocks

(defmacro do!
  "Defines an imperative code block, where you can declare block-scoped local
   mutable variables using `(var <name> <value>)`. The variables can be mutated
   using the other assignemnt operators such as `!=`, `+=`, `-=`, `++`, `--`,
   etc. For assignment you can also use `(set! <name> <new-val>)`."
  {:style/indent 0}
  [& body]
  `(var-scope ~@body))

;;;; Blocks
;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;;;; Loops

(def ^:const _ nil)

(defn break
  "Breaks out of for! loop. Can be called inside a function call as long
   as its within the scope and extent of the for! loop."
  {:inline (fn [] `(throw loop-impl/ex-break))}
  []
  (throw loop-impl/ex-break))

(defn continue
  "Continues to next iteration skipping over what's left in current pass.
   Can be called inside a function call as long as its within the scope
   and extent of the for! loop."
  {:inline (fn [] `(throw loop-impl/ex-continue))}
  []
  (throw loop-impl/ex-continue))

(s/fdef for!
  :args (s/cat :init (s/or :list list?
                           :vector (s/coll-of list?)
                           :skip #(and (symbol? %)
                                       (= (name %) "_")))
               :condition list?
               :mutation (s/or :list list?
                               :vector (s/coll-of list?)
                               :skip #(and (symbol? %)
                                           (= (name %) "_")))
               :body (s/? (s/* any?)))
  :ret any?)

(defmacro for!
  "Imperative for loop, always returns nil, assumes side-effects.

   Takes an `init` `condition` `mutation` argument triplet
     (var i = 0; i < 10; i++) -> (var i 0) (< i 10) (++ i)
   Can also take multiple init/mutation by wrapping in extra vector
     (var i = 0, var j = 10; i < 10 && j > 0; i++, j--)
     -> [(var i 0) (var j 10)] (and (< i 10) (> j 0)) [(++ i) (-- j)]

   Rest is body of loop.

   Just like in an imperative for loop, you can break out of the loop early by
   calling the `(break)` function, or skip to the next iteration by calling the
   `(continue)` function from anywhere inside the body, or inside a function
   called by the body of the loop.

   Example:
     (for! [(var i 0) (var j 10)] (and (< i 10) (> j 0)) [(++ i) (-- j)]
       (when (= 3 i) (continue))
       (when (= 5 i) (break))
       (println i j))"
  {:style/indent 3}
  [init condition mutation & body]
  (loop-impl/for-loop init condition mutation body))

(defmacro while!
  "Imperative while loop, always returns nil, assumes side-effects.

   First argument is test condition, will loop until test condition
   evaluates to false.

   Rest is body of loop.

   Just like in an imperative while loop, you can break out of the loop
   early by calling the `(break)` function, or skip to the next iteration
   by calling the `(continue)` function from anywhere inside the body, or
   inside a function called by the body of the loop.

   Example:
     (var i 0)
     (while! (< i 10)
       (when (= i 4)
         (++ i)
         (continue))
       (println i)
       (++ i)
       (when (= i 8)
         (break)))"
  {:style/indent 1}
  [condition & body]
  (loop-impl/while-loop condition body))

(defmacro do-while!
  "Like while!, but condition is evaluated after body. Mimics an imperative
   do while loop."
  {:style/indent 1}
  [condition & body]
  (loop-impl/do-while-loop condition body))

;;;; Loops
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;
;;;; Assignments

(defmacro !=
  "Simple assignment operator. Assigns value `e` to variable `v`."
  {:style/indent 1}
  [v e]
  `(set! ~v ~e))

(defmacro +=
  "Add AND assignment operator. It adds value `e` to the value of variable `v`
   and assigns the result back into variable `v`."
  {:style/indent 1}
  [v e]
  `(set! ~v (+ ~v ~e)))

(defmacro -=
  "Substract AND assignment operator. It substracts value `e` from the value of
   variable `v` and assigns the result back into variable `v`."
  {:style/indent 1}
  [v e]
  `(set! ~v (- ~v ~e)))

(defmacro *=
  "Multiply AND assignment operator. It multiplies value `e` with the value of
   variable `v` and assigns the result back into variable `v`."
  {:style/indent 1}
  [v e]
  `(set! ~v (* ~v ~e)))

(defmacro div=
  "Divide AND assignment operator. It divides the value of variable `v` by value
   `e` and assigns the result back into variable `v`."
  {:style/indent 1}
  [v e]
  `(set! ~v (/ ~v ~e)))

(defmacro quot=
  "Quotient AND assignment operator. It divides the value of variable `v` by value
   `e` and assigns the quotient back into variable `v`."
  {:style/indent 1}
  [v e]
  `(set! ~v (quot ~v ~e)))

(defmacro rem=
  "Remainder AND assignment operator. It divides the value of variable `v` by
   value `e` and assigns the remainder back into variable `v`. Also knows as
   modulus or mod operator in Java."
  {:style/indent 1}
  [v e]
  `(set! ~v (rem ~v ~e)))

(defmacro bitand=
  "Bitwise AND assignment operator."
  {:style/indent 1}
  [v e]
  `(set! ~v (bit-and ~v ~e)))

(defmacro bitxor=
  "Bitwise exclusive OR and assignment operator."
  {:style/indent 1}
  [v e]
  `(set! ~v (bit-xor ~v ~e)))

(defmacro bitor=
  "Bitwise inclusive OR and assignment operator."
  {:style/indent 1}
  [v e]
  `(set! ~v (bit-or ~v ~e)))

(defmacro bitleft=
  "Left shift AND assignment operator."
  {:style/indent 1}
  [v e]
  `(set! ~v (bit-shift-left ~v ~e)))

(defmacro bitright=
  "Right shift AND assignment operator."
  {:style/indent 1}
  [v e]
  `(set! ~v (bit-shift-right ~v ~e)))

(defmacro unbitright=
  "Unsigned	right shift AND assignment operator."
  {:style/indent 1}
  [v e]
  `(set! ~v (unsigned-bit-shift-right ~v ~e)))

;;;; Assignments
;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;;;; Unary

(defmacro ++
  "Increment operator. Increases the value of variable `v` by 1."
  {:style/indent 1}
  [v]
  `(set! ~v (inc ~v)))

(defmacro --
  "Decrement operator. Decreases the value of variable `v` by 1."
  {:style/indent 1}
  [v]
  `(set! ~v (dec ~v)))

;;;; Unary
;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;
;;;; Conditionals

(defmacro when!
  "When `test` evaluates to truthy, evaluates `body` where `body` can contain
   mutable variables."
  {:style/indent 1}
  [test & body]
  `(when ~test (var-scope ~@body)))

(defmacro when-not!
  "When `test` evaluates to falsy, evaluates `body` where `body` can contain
   mutable variables."
  {:style/indent 1}
  [test & body]
  `(when-not ~test (var-scope ~@body)))

;;;; Conditionals
;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;
;;;; Procedures

(defn return
  "Returns out of a procedure defn!/fn!. Can be called inside a function call as
   long as its within the scope and extent of the procedure defn!/fn!"
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
  "Imperative procedure. It differs from a Clojure defn in that it allows the
   use of local mutable variables using `(var <name> <value>) and other
   accompanying operators like `!=`, `+=`, `-=`, `++`, `--`, etc."
  {:style/indent [:defn]}
  [& args]
  (let [conf (proc-impl/conform-defn args)
        new-conf (proc-impl/update-conf conf (partial proc-impl/wrap-var-scope))
        new-args (proc-impl/unform-defn new-conf)]
    (cons `defn new-args)))

(defmacro fn!
  "Imperative procedure. It differs from a Clojure fn in that it allows the use
   of local mutable variables using `(var <name> <value>) and other accompanying
   operators like `!=`, `+=`, `-=`, `++`, `--`, etc."
  {:style/indent [:defn]}
  [& args]
  (let [conf (proc-impl/conform-fn args)
        new-conf (proc-impl/update-conf conf (partial proc-impl/wrap-var-scope))
        new-args (proc-impl/unform-fn new-conf)]
    (cons `fn new-args)))

;;;; Procedures
;;;;;;;;;;;;;;;;;;;
