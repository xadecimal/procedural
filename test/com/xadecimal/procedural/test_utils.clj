(ns com.xadecimal.procedural.test-utils
  (:require [com.xadecimal.procedural.var-scope :as vs]))

(defmacro get-local-class-info
  "Returns the class info of the provided local-sym, assumes
   a local exists with symbol equal to local-sym.

   Example: (let [i (+ (int 5) (float 10))]
              (get-local-class-info))
   Returns: {:class float, :primitive? true}"
  [local-sym]
  (#'vs/get-local-binding-info (get &env local-sym)))

(defmacro expression-infom
  "Macro that puts expr into a let form, and uses local &env analysis to analyze
   the expression. Returns a map with keys :class and :primitive? indicating
   what the compiler concluded about the return value of the expression. Returns
   nil if no type info can be determined at compile-time.

   Example: (expression-info (+ (int 5) (float 10)))
   Returns: {:class float, :primitive? true}"
  [expr]
  `(let [expr# ~expr]
     (get-local-class-info expr#)))
