(ns com.xadecimal.procedural.var-scope-test
  (:require
   [clojure.test :as test :refer [deftest is testing]]
   [com.xadecimal.procedural.test-utils :refer [expression-infom]]
   [com.xadecimal.procedural.var-scope :refer [var-scope]]))

(def this-ns *ns*)

(defn- eval-here
  [form]
  (binding [*ns* this-ns]
    (eval form)))

(deftest test1
  (testing
      "Define a mutable var and read its value."
    (is (= (var-scope
            (var i 10)
            i)
           10))))

(deftest test2
  (testing
      "Assign a new value to it, mutating its existing value."
    (is (= (var-scope
            (var i 10)
            (set! i 100)
            i)
           100))))

(deftest test3
  (testing
      "Define more than one mutable var."
    (is (= (var-scope
            (var i 10)
            (var j 100)
            [i j])
           [10 100]))))

(deftest test4
  (testing
      "You can define a mutable var anywhere in the scope,
it does not need to be at the start."
    (is (= (var-scope
            (+ 1 2)
            (var i 10)
            i)
           10))))

(deftest test5
  (testing
      "Even if you have many of them."
    (is (= (var-scope
            (+ 1 2)
            (var i 10)
            (- 100 20)
            (var j 100)
            (+ 0 0)
            [i j])
           [10 100]))))

(deftest test6
  (testing
      "They can still be mutated as you want."
    (is (= (var-scope
            (+ 1 2)
            (var i 10)
            (- 100 20)
            (var j 100)
            (set! i 0)
            (+ 0 0)
            (set! j 1)
            [i j])
           [0 1]))))

(deftest test7
  (testing
      "You can assign the result of an expression, not just literal values."
    (is (= (var-scope
            (var i (+ 1 2 3 4))
            i)
           10))))

(deftest test8
  (testing
      "Same goes when re-assigning."
    (is (= (var-scope
            (var i (+ 1 2 3 4))
            (set! i (- 1 2 3 4))
            i)
           -8))))

(deftest test9
  (testing
      "You can also assign from a binding."
    (is (= (var-scope
            (var i 0)
            (let [x 10]
              (set! i x))
            i)
           10))))

(deftest test10
  (testing
      "Or from inside an anonymous function (lambda)."
    (is (= (var-scope
            (var i 0)
            (#(set! i 10))
            i)
           10))))

(deftest test11
  (testing
      "In which case the value of the var will be set when the lambda executes,
  not when it is defined."
    (is (= (var-scope
            (var i 0)
            (let [set-i #(set! i 10)]
              (is (= i 0))
              (set-i)
              i))
           10))))

(deftest test12
  (testing
      "You can also read the value from inside an anonymous function (lambda)."
    (is (= (var-scope
            (var i 10)
            (#(+ 100 i)))
           110))))

(deftest test13
  (testing
      "You have to be careful though, the mutable var will contain the value at
  the time the lambda is executed, not at the time it is defined."
    (is (= (var-scope
            (var i 0)
            (let [lambda (fn [] (+ 100 i))]
              (is (= (lambda) 100))
              (set! i 10)
              (lambda)))
           110))))

(deftest test14
  (testing
      "If you need to use Clojure's `var` special form, you can still do so as
  as normal, we simply overloaded it so the 2-ary call defines a local
  mutable var, but the 1-ary is still the standard Clojure var special form."
    (is (= (def a 1)
           (var a)
           #'a))))

(deftest test15
  (testing
      "You cannot use var without initializing it to a value at the same time,
  thus it is not possible to just declare a var without initializing it."
    ;; Wrapped in eval-here in order to catch a compiler exception.
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (var i)))))))

(deftest test16
  (testing
      "Remember, using var as a 1-ary invokes the standard Clojure var special
  form instead, so all use without an initial value reverts to the var
  special form standard behavior."
    (is (= (var-scope
            (def a 1)
            (var a))
           #'a))))

(deftest test17
  (testing
      "Variables are scoped to their block only, meaning within the var-scope form
  and inner forms, but no longer are available when outside the var-scope they
  were defined."
    (is (= (var-scope
            (var i 10))
           (resolve 'i)
           nil))))

(deftest test18
  (testing
      "You can nest var-scopes inside another, in which case inner scopes will see
  variables from outer scopes and can set! and read them."
    (is (= (var-scope
            (var i 0)
            (var-scope
             (var j 1)
             (is (= [i j] [0 1]))
             (set! i 10)
             (set! j 11)
             (is (= [i j] [10 11])))
            (is (= (resolve 'j) nil))
            i)
           10))))

(deftest test19
  (testing
      "Inner scopes shadow var names from outer scopes, meaning inside the inner
  scope it refers to the var defined in that inner scope, and in the outer
  scope it refers to the var defined in that outer scope."
    (is (= (var-scope
            (var i 10)
            (var-scope
             (var i 100)
             (is (= i 100)))
            i)
           10))))

(deftest test20
  (testing
      "You're not allowed to define the same var twice in the same scope."
    ;; Wrapped in eval-here in order to catch a compile exception.
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (var i 10)
                    (var i 100)))))
    (try
      (eval-here
       '(var-scope
         (var i 10)
         (var i 100)))
      (catch Exception e
        (let [edata (ex-data (.getCause e))]
          (is (= (:ex-type edata) :invalid-input))
          (is (= (:input edata) '(var i 100)))
          edata)))))

(deftest test21
  (testing
      "You cannot set! a var before it has been defined."
    ;; Wrapped in eval-here in order to catch a compile exception.
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (set! i 100)
                    (var i 0)))))))

(deftest test22
  (testing
      "You cannot define a var from inside a closure."
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (#(var i 0))
                    i))))))

(deftest test23
  (testing
      "You cannot refer to a variable declared later prior to it being declared."
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (+ 1 i)
                    (var i 10)))))))

(deftest test24
  (testing
      "Even from within a closure."
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    #(+ 1 i)
                    (var i 10)))))))

(deftest test25
  (testing
      "Var will be a primitive value if it infers that you're assigning to it a
  primitive. It uses Clojure's compiler type hint inference to do so,
  same as that of let/loop."
    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i 0)
              i))
            str)
           {:class "long" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a 0]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "long" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a 1.0]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "double" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a true]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "boolean" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a (int 1)]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "int" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a (float 1.0)]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "float" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a (short 1)]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "short" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a (byte 1)]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "byte" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (let [a (char 1)]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "char" :primitive? "true"}))))

(deftest test26
  (testing
      "Otherwise it will of type Object."
    (is (= (update-vals
            (expression-infom
             (let [a nil]
               (var-scope
                (var i a)
                i)))
            str)
           {:class "class java.lang.Object" :primitive? "false"}))))

(deftest test27
  (testing
      "Similar to let/loop, if the type can't be inferred, you can provide an
  explicit type hint or wrap the value in one of the primitive cast functions."
    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i ^long (#(+ 1 2)))
              i))
            str)
           {:class "long" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i (long (#(+ 1 2))))
              i))
            str)
           {:class "long" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i ^int (#(+ 1 2)))
              i))
            str)
           {:class "int" :primitive? "true"}))

    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i (int (#(+ 1 2))))
              i))
            str)
           {:class "int" :primitive? "true"}))))

(deftest test28
  (testing
      "Do not type hint the variable name, type hint the value instead."
    (is (= (update-vals
            (expression-infom
             (var-scope
              (var ^long i (#(+ 1 2)))
              i))
            str)
           {}))

    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i ^long (#(+ 1 2)))
              i))
            str)
           {:class "long" :primitive? "true"}))))

(deftest test29
  (testing
      "You can't change the type of a var after it's been defined. If you
  try to set! a value of the wrong type it will be a runtime error."
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (var i 0)
                    (set! i "hello")))))

    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (var i 0)
                    (var i "hello")))))))

(deftest test30
  (testing
      "You can't define the same var twice in the same scope."
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (var i 0)
                    (var i "hello")))))))

(deftest test31
  (testing
      "If you don't want to use a primitive type, because you'd like to
  store nil, or you'd like to be able to set! any type in the variable,
  use `num` to cast to a boxed number or type hint as `Object` or wrap in
  the `identity` function. `idendity` function works in all cases, while
  `num` only works for primitive numbers, and type hinting to Object
  only works for function's return value."
    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i (num 0))
              i))
            str)
           {:class "class java.lang.Object" :primitive? "false"}))

    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i ^Object (+ 1 2))
              i))
            str)
           {:class "class java.lang.Object" :primitive? "false"}))

    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i (identity false))
              i))
            str)
           {:class "class java.lang.Object" :primitive? "false"}))))

(deftest test32
  (testing
      "Vars shadow let bindings as well."
    (is (= (eval-here
            '(let [i 10]
               (var-scope
                (is (= i 10))
                (var i i)
                (set! i 100)
                i)))
           100))))

(deftest test33
  (testing
      "You can't define a var inside a nested let, or any nested form really.
       Unless they're also a var-scope form."
    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (let [x 10]
                      (var i x))
                    i))))

    (is (thrown? Exception
                 (eval-here
                  '(var-scope
                    (for [x [1]]
                      (var i x))
                    i))))))

(deftest test34
  (testing
      "If you initialize a var with the value of another, it will adopt the same
  type."
    (is (= (update-vals
            (expression-infom
             (var-scope
              (var i (short 0))
              (var j i)
              j))
            str)
           {:class "short" :primitive? "true"}))))

(deftest test35
  (testing
      "You're allowed to set! inside another expression, and set! returns the
  newly set! value."
    (is (= (var-scope
            (var i 10)
            (is (= (+ 10 (set! i 20)) 30))
            i)
           20))))

(deftest test36
  (testing
      "You can nest var-scope inside other forms inside a var-scope."
    (is (= (var-scope
            (var i 0)
            (when (= i 0)
              (var-scope
               (var i 10)
               (is (= i 10))))
            i)
           0))))

(def ^{:tag 'int} k 10)

(deftest test37
  (testing
      "A primitive type hinted global Var should have it's type properly inferred
       even when indirectly accessed through an intermediate let binding. And a
       complicated chain of intermediate re-binding to more vars should maintain
       the type hint throughout."
    (is (= (let [i k]
             (var-scope
              (is (= (str i) "10"))
              (var i i)
              (is (= (expression-infom i) {:class Integer/TYPE, :primitive? true}))
              (is (= i 10))
              (var j i)
              (is (= (expression-infom j) {:class Integer/TYPE, :primitive? true}))
              (is (= j 10))
              (set! j 125)
              (is (= (expression-infom j) {:class Integer/TYPE, :primitive? true}))
              (is (= j 125))
              (is (= (str j) "125"))
              (var-scope
               (is (= j 125))
               (is (= (expression-infom j) {:class Integer/TYPE, :primitive? true}))
               (var i 200)
               (is (= (expression-infom i) {:class Long/TYPE, :primitive? true}))
               (is (= i 200))
               (var j j)
               (is (= (expression-infom j) {:class Integer/TYPE, :primitive? true}))
               [i j])))
           [200 125]))))
