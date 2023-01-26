(ns com.xadecimal.procedural
  (:require [clojure.string :as str]
            [clojure.test :as test]
            [com.xadecimal.procedural.common :refer [expression-info]]
            [com.xadecimal.procedural.var-scope :refer [var-scope]]
            [hyperfiddle.rcf :as rcf]
            [clojure.zip :as zip]))

;; TODO: Refactor var-scope so you can only use var directly inside var-scope,
;; at the same level, and not nested inside inner forms (unless they are also
;; a var-scope)

;; Override rcf reporter so it reports each test on its own line.
(defmethod test/report :hyperfiddle.rcf/pass [_m]
  (test/with-test-out
    (test/inc-report-counter :pass)
    (println "✅")
    (flush)))

;; Override rcf reporter so it adds X to tests that fail with an error
(defmethod test/report :error [m]
  (print "🔥 ")
  (print (str/triml (with-out-str
                      (binding [test/*test-out* *out*]
                        (test/report (-> m
                                         (assoc :type :fail)
                                         (assoc :actual
                                                #:exception{:type (type (:actual m))
                                                            :msg (ex-message (:actual m))}))))))))

(defmacro tests
  [& body]
  `(do (rcf/tests ~@body)
       nil))

(tests
 "Define a mutable var and read its value."
 (var-scope
  (var i 10)
  i)
 := 10)

(tests
 "Assign a new value to it, mutating its existing value."
 (var-scope
  (var i 10)
  (set! i 100)
  i)
 := 100)

(tests
 "Define more than one mutable var."
 (var-scope
  (var i 10)
  (var j 100)
  [i j])
 := [10 100])

(tests
 "You can define a mutable var anywhere in the scope,
it does not need to be at the start."
 (var-scope
  (+ 1 2)
  (var i 10)
  i)
 := 10)

(tests
 "Even if you have many of them."
 (var-scope
  (+ 1 2)
  (var i 10)
  (- 100 20)
  (var j 100)
  (+ 0 0)
  [i j])
 := [10 100])

(tests
 "They can still be mutated as you want."
 (var-scope
  (+ 1 2)
  (var i 10)
  (- 100 20)
  (var j 100)
  (set! i 0)
  (+ 0 0)
  (set! j 1)
  [i j])
 := [0 1])

(tests
 "You can assign the result of an expression, not just literal values."
 (var-scope
  (var i (+ 1 2 3 4))
  i)
 := 10)

(tests
 "Same goes when re-assigning."
 (var-scope
  (var i (+ 1 2 3 4))
  (set! i (- 1 2 3 4))
  i)
 := -8)

(tests
 "You can also assign from a binding."
 (var-scope
  (var i 0)
  (let [x 10]
    (set! i x))
  i)
 := 10)

(tests
 "Or from inside an anonymous function (lambda)."
 (var-scope
  (var i 0)
  (#(set! i 10))
  i)
 := 10)

(tests
 "In which case the value of the var will be set when the lambda executes,
  not when it is defined."
 (var-scope
  (var i 0)
  (let [set-i #(set! i 10)]
    i := 0
    (set-i)
    i))
 := 10)

(tests
 "You can also read the value from inside an anonymous function (lambda)."
 (var-scope
  (var i 10)
  (#(+ 100 i)))
 := 110)

(tests
 "You have to be careful though, the mutable var will contain the value at
  the time the lambda is executed, not at the time it is defined."
 (var-scope
  (var i 0)
  (let [lambda (fn [] (+ 100 i))]
    (lambda) := 100
    (set! i 10)
    (lambda)))
 := 110)

(tests
 "If you need to use Clojure's `var` special form, you can still do so as
  as normal, we simply overloaded it so the 2-ary call defines a local
  mutable var, but the 1-ary is still the standard Clojure var special form."
 (def a 1)
 (var a)
 := #'a)

(tests
 "You cannot use var without initializing it to a value at the same time,
  thus it is not possible to just declare a var without initializing it."
 ;; Wrapped in eval in order to catch a compiler exception.
 (eval
  '(var-scope
    (var i)))
 :throws Exception)

(tests
 "Remember, using var as a 1-ary invokes the standard Clojure var special
  form instead, so all use without an initial value reverts to the var
  special form standard behavior."
 (var-scope
  (def a 1)
  (var a))
 := #'a)

(tests
 "Variables are scoped to their block only, meaning within the var-scope form
  and inner forms, but no longer are available when outside the var-scope they
  were defined."
 (var-scope
  (var i 10))
 (resolve 'i) := nil)

(tests
 "You can nest var-scopes inside another, in which case inner scopes will see
  variables from outer scopes and can set! and read them."
 (var-scope
  (var i 0)
  (var-scope
   (var j 1)
   [i j] := [0 1]
   (set! i 10)
   (set! j 11)
   [i j] := [10 11])
  (resolve 'j) := nil
  i)
 := 10)

(tests
 "Inner scopes shadow var names from outer scopes, meaning inside the inner
  scope it refers to the var defined in that inner scope, and in the outer
  scope it refers to the var defined in that outer scope."
 (var-scope
  (var i 10)
  (var-scope
   (var i 100)
   i := 100)
  i)
 := 10)

(tests
 "You're not allowed to define the same var twice in the same scope."
 ;; Wrapped in eval in order to catch a compile exception.
 (eval
  '(var-scope
    (var i 10)
    (var i 100)))
 :throws Exception)

(tests
 "You cannot set! a var before it has been defined."
 ;; Wrapped in eval in order to catch a compile exception.
 (eval
  '(var-scope
    (set! i 100)
    (var i 0)))
 :throws Exception)

(tests
 "You cannot define a var from inside a closure."
 (eval
  '(var-scope
    (#(var i 0))
    i))
 :throws Exception)

(tests
 "You cannot refer to a variable declared later prior to it being declared."
 (eval
  '(var-scope
    (+ 1 i)
    (var i 10)))
 :throws Exception)

(tests
 "Even from within a closure."
 (eval
  '(var-scope
    #(+ 1 i)
    (var i 10)))
 :throws Exception)

(tests
 "Var will be a primitive value if it infers that you're assigning to it a
  primitive. It uses Clojure's compiler type hint inference to do so,
  same as that of let/loop."
 (update-vals
  (expression-info
   '(var-scope
     (var i 0)
     i))
  str)
 := {:class "long" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a 0]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "long" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a 1.0]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "double" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a true]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "boolean" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a (int 1)]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "int" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a (float 1.0)]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "float" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a (short 1)]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "short" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a (byte 1)]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "byte" :primitive? "true"}

 (update-vals
  (expression-info
   '(let [a (char 1)]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "char" :primitive? "true"})

(tests
 "Otherwise it will of type Object."
 (update-vals
  (expression-info
   '(let [a nil]
      (var-scope
       (var i a)
       i)))
  str)
 := {:class "class java.lang.Object" :primitive? "false"})

(tests
 "Similar to let/loop, if the type can't be inferred, you can provide an
  explicit type hint or wrap the value in one of the primitive cast functions."
 (update-vals
  (expression-info
   '(var-scope
     (var i ^long (#(+ 1 2)))
     i))
  str)
 := {:class "long" :primitive? "true"}

 (update-vals
  (expression-info
   '(var-scope
     (var i (long (#(+ 1 2))))
     i))
  str)
 := {:class "long" :primitive? "true"}

 (update-vals
  (expression-info
   '(var-scope
     (var i ^int (#(+ 1 2)))
     i))
  str)
 := {:class "int" :primitive? "true"}

 (update-vals
  (expression-info
   '(var-scope
     (var i (int (#(+ 1 2))))
     i))
  str)
 := {:class "int" :primitive? "true"})

(tests
 "Do not type hint the variable name, type hint the value instead."
 (update-vals
  (expression-info
   '(var-scope
     (var ^long i (#(+ 1 2)))
     i))
  str)
 := {}

 (update-vals
  (expression-info
   '(var-scope
     (var i ^long (#(+ 1 2)))
     i))
  str)
 := {:class "long" :primitive? "true"})

(tests
 "You can't change the type of a var after it's been defined. If you
  try to set! a value of the wrong type it will be a runtime error."
 (eval
  '(var-scope
    (var i 0)
    (set! i "hello")))
 :throws Exception

 (eval
  '(var-scope
    (var i 0)
    (var i "hello")))
 :throws Exception)

(tests
 "You can't define the same var twice in the same scope."
 (eval
  '(var-scope
    (var i 0)
    (var i "hello")))
 :throws Exception)

(tests
 "If you don't want to use a primitive type, because you'd like to
  store nil, or you'd like to be able to set! any type in the variable,
  use `num` to cast to a boxed number or type hint as `Object` or wrap in
  the `identity` function. `idendity` function works in all cases, while
  `num` only works for primitive numbers, and type hinting to Object
  only works for function's return value."
 (update-vals
  (expression-info
   '(var-scope
     (var i (num 0))
     i))
  str)
 := {:class "class java.lang.Object" :primitive? "false"}

 (update-vals
  (expression-info
   '(var-scope
     (var i ^Object (+ 1 2))
     i))
  str)
 := {:class "class java.lang.Object" :primitive? "false"}

 (update-vals
  (expression-info
   '(var-scope
     (var i (identity false))
     i))
  str)
 := {:class "class java.lang.Object" :primitive? "false"})

;; TODO: I might not be able to make this work, so maybe you should not be
;; allowed to shadow a let binding?
(tests
 "Vars shadow let bindings as well."
 (eval
  '(let [i 10]
     (var-scope
      i := 10
      (var i i)
      (set! i 100)
      i)))
 := 100)

(tests
 "You can't define a var inside a nested let, or any nested form really."
 (eval
  '(var-scope
    (let [x 10]
      (var i x))
    i))
 :throws Exception

 (eval
  '(var-scope
    (for [x [1]]
      (var i x))
    i))
 :throws Exception)

(tests
 "If you initialize a var with the value of another, it will adopt the same
  type."
 (update-vals
  (expression-info
   '(var-scope
     (var i (short 0))
     (var j i)
     j))
  str)
 := {:class "short" :primitive? "true"})

(tests
 "You're allowed to set! inside another expression, and set! returns the
  newly set! value."
 (var-scope
  (var i 10)
  (+ 10 (set! i 20)) := 30
  i)
 := 20)

(tests
 "You can nest var-scope inside other forms inside a var-scope."
 (var-scope
  (var i 0)
  (when (= i 0)
    (var-scope
     (var i 10)
     i := 10))
  i)
 := 0)

(tests
 (let [stack (atom '())
       new-list (atom '())
       input '(1 2 :nest 3 4 :nest 5 6 7 :nest 8)]
   (doseq [e input] (swap! stack conj e))
   (while (seq @stack)
     (let [e (ffirst (swap-vals! stack pop))]
       (if (not= :nest e)
         (swap! new-list #(cons e %))
         (do (swap! stack conj @new-list)
             (reset! new-list '())))))
   @new-list)
 := '(1 2 (3 4 (5 6 7 (8))))

 ((fn nest-seq [coll]
    (when (seq coll)
      (let [[f & r] coll]
        (if (= f :nest)
          (list (nest-seq r))
          (cons f (nest-seq r))))))
  '(1 2 :nest 3 4 :nest 5 6 7 :nest 8))
 := '(1 2 (3 4 (5 6 7 (8))))

 (let [zip (zip/seq-zip '(1 2 :nest 3 4 :nest 5 6 7 :nest 8))]
   (loop [zip (zip/down zip)]
     (cond (= (zip/node zip) :nest)
           (recur
            (zip/down
             (assoc-in
              (zip/edit zip #(rest (cons % (zip/rights zip))))
              [1 :r] nil)))
           (zip/right zip)
           (recur (zip/right zip))
           :else
           (zip/root zip))))
 := '(1 2 (3 4 (5 6 7 (8)))))
