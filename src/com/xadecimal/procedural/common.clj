(ns com.xadecimal.procedural.common
  (:import [com.xadecimal.procedural ExBreak ExContinue]
           [clojure.lang Compiler Compiler$C Compiler$FnExpr
            Compiler$BodyExpr Compiler$FnMethod]))

(def ex-break (ExBreak.))
(def ex-continue (ExContinue.))

(defn expression-info
  "Uses the Clojure compiler to analyze the given s-expr. Returns
   a map with keys :class and :primitive? indicating what the compiler
   concluded about the return value of the expression. Returns nil if
   no type info can be determined at compile-time.

   Example: (expression-info '(+ (int 5) (float 10)))
   Returns: {:class float, :primitive? true}"
  [expr]
  (let [^Compiler$FnExpr fn-ast (Compiler/analyze Compiler$C/EXPRESSION `(fn [] ~expr))
        ^Compiler$BodyExpr expr-ast (.body ^Compiler$FnMethod (first (.methods fn-ast)))]
    (when (.hasJavaClass expr-ast)
      {:class (.getJavaClass expr-ast)
       :primitive? (some-> expr-ast .getJavaClass .isPrimitive)})))

(defn contains?v
  "Returns true if vector v contains element e, false otherwise."
  [v e]
  (.contains ^java.util.List v e))

(defn conj-distinctv
  "Conj-oins element e into vector v where e is distinct, meaning if it is
   already present in v it will not be duplicated."
  [v e]
  (into [] (distinct) (conj v e)))
