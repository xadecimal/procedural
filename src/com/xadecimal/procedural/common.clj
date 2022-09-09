(in-ns 'com.xadecimal.procedural)

(def ^:private ex-break (ExBreak.))
(def ^:private ex-continue (ExContinue.))

(defn- prewalk
  "Like prewalk, but skips forms that are reduced, that
   way you can prevent walking inside a returned form
   by having f return it reduced, final result will
   deref the reduced form and return form without reduced."
  [f form]
  (walk/walk
   (partial prewalk f)
   #(if (reduced? %) (deref %) %)
   (f form)))

(defn- expression-info
  "Uses the Clojure compiler to analyze the given s-expr.  Returns
  a map with keys :class and :primitive? indicating what the compiler
  concluded about the return value of the expression.  Returns nil if
  not type info can be determined at compile-time.

  Example: (expression-info '(+ (int 5) (float 10)))
  Returns: {:class float, :primitive? true}"
  [expr]
  (let [^Compiler$FnExpr fn-ast (Compiler/analyze Compiler$C/EXPRESSION `(fn [] ~expr))
        ^Compiler$BodyExpr expr-ast (.body ^Compiler$FnMethod (first (.methods fn-ast)))]
    (when (.hasJavaClass expr-ast)
      {:class (.getJavaClass expr-ast)
       :primitive? (some-> expr-ast .getJavaClass .isPrimitive)})))

(defn- contains?v
  [v e]
  (.contains ^java.util.List v e))

(defn- conj-distinctv
  [v e]
  (into [] (distinct) (conj v e)))
