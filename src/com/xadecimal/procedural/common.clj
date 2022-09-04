(in-ns 'com.xadecimal.procedural)

(def ex-break (ExBreak.))
(def ex-continue (ExContinue.))

(defn prewalk
  "Like prewalk, but skips forms that are reduced, that
   way you can prevent walking inside a returned form
   by having f return it reduced, final result will
   deref the reduced form and return form without reduced."
  [f form]
  (walk/walk
   (partial prewalk f)
   #(if (reduced? %) (deref %) %)
   (f form)))
