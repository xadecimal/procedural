(in-ns 'com.xadecimal.procedural)

(defn- insert-throws
  [body]
  (prewalk
   (fn[form]
     (cond (and (list? form) (= 'break (first form)))
           (reduced `(throw ex-break))
           (and (list? form) (= 'continue (first form)))
           (reduced `(throw ex-continue))
           :else
           form))
   body))

(defn- for-to-loop
  [[s i c p] body]
  `(try
     (loop ~(if (vector? s) s [s i])
       (when ~(if (vector? s) i c)
         (try
           ~@(insert-throws body)
           (catch ExContinue t#))
         ~(if (vector? s) `(recur ~@c) `(recur ~p))))
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
