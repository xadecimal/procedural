(ns com.xadecimal.procedural-test
  (:require [com.xadecimal.procedural :refer :all]
            [clojure.test :refer [deftest is testing]]))

(deftest test-do!
  (testing "Local mutable variables can be used inside do!"
    (is (= 10 (do! (var x 10)
                   x))))
  (testing "Their values can be mutated using the other assignment ops."
    (is (= 100 (do! (var x 10)
                    (*= x 4)
                    (+= x 60)))))
  (testing "You can nest a do! inside a do!, variables will shadow each other."
    (is (= 100 (do! (var x 10)
                    (do! (var x 100)
                         x)))))
  (testing "You can't change the infered type of a var after initialization."
    (is (thrown? Exception (do! (var x 1)
                                (!= x "1"))))))

(deftest test-for!
  (testing "For! loop iterates correctly with increment operation."
    (let [result (atom 0)]
      (for! (var i 0) (< i 5) (++ i)
        (swap! result + i))
      (is (= 10 @result))))
  (testing "For! loop supports 'continue' and 'break' control statements."
    (let [result (atom 0)]
      (for! (var i 0) (< i 5) (++ i)
        (when (= i 1) (continue))
        (when (= i 3) (break))
        (swap! result + i))
      (is (= 2 @result))))
  (testing "For! loop supports inner loops"
    (let [result (atom 0)]
      (for! (var i 0) (< i 5) (++ i)
        (for! (var j 0) (< j i) (++ j)
          (swap! result + j)))
      (is (= 10 @result))))
  (testing "For! loop supports multiple init and mutation"
    (let [result (atom 0)]
      (for! [(var i 0) (var j 4)] (and (< i 5) (>= j 0)) [(++ i) (-- j)]
        (swap! result + i)
        (swap! result - j))
      (is (= 0 @result))))
  (testing "Can declare mutable variables inside body of for!"
    (let [result (atom 0)]
      (for! (var i 0) (< i 5) (++ i)
        (var x 1)
        (swap! result + x))
      (is (= 5 @result))))
  (testing "For loop raises exception when body throws."
    (is (thrown? Exception
                 (for! (var i 0) (< i 5) (++ i)
                   (throw (Exception. "Error")))))))

(deftest test-while!
  (testing "While! loop performs as expected with basic increment operation."
    (do! (var result 0)
         (while! (< result 5)
           (++ result))
         (is (= 5 result))))
  (testing "While! loop supports 'break' control statement to exit early."
    (do! (var result 0)
         (while! (< result 10)
           (++ result)
           (when (= result 3) (break)))
         (is (= 3 result))))
  (testing "While! loop supports 'continue' control statement."
    (do! (var result 0)
         (var acc 0)
         (while! (< result 5)
           (++ result)
           (when (= 2 result) (continue))
           (+= acc result))
         (is (= 13 acc))))
  (testing "Can declare mutable variables inside body of while!"
    (let [result (atom 0)]
      (do!
        (var i 0)
        (while! (< i 5)
          (var x 1)
          (swap! result + x)
          (++ i)))
      (is (= 5 @result))))
  (testing "While loop raises exception with infinite loop containing throw."
    (is (thrown? Exception (while! true (throw (Exception. "Infinite loop")))))))

(deftest test-do-while!
  (testing "Do-while! loop executes at least once before condition check."
    (do! (var result 0)
         (do-while! false
           (++ result))
         (is (= 1 result))))
  (testing "Do-while! loop supports 'break' control statement to exit early."
    (do! (var result 0)
         (do-while! (< result 10)
           (++ result)
           (when (= result 3) (break)))
         (is (= 3 result))))
  (testing "Do-while! loop supports 'continue' control statement."
    (do! (var result 0)
         (var acc 0)
         (do-while! (< result 5)
           (++ result)
           (when (= 2 result) (continue))
           (+= acc result))
         (is (= 13 acc))))
  (testing "Do-while! loop properly handles infinite loop scenario with throw."
    (is (thrown? Exception (do-while! true (throw (Exception. "Infinite loop"))))))
  (testing "Can declare mutable variables inside body of do-while!"
    (let [result (atom 0)]
      (do!
        (var i 0)
        (do-while! (< i 5)
          (var x 1)
          (swap! result + x)
          (++ i)))
      (is (= 5 @result)))))

(deftest test-assignments-and-unary-operators
  (testing "Variable mutations are correctly applied through various operators."
    (do! (var x 10)
         (!= x 20)
         (+= x 5)
         (-= x 2)
         (*= x 2)
         (quot= x 4)
         (rem= x 3)
         (++ x)
         (is (= 3 x))))
  (testing "Bitwise operations correctly mutate the variable."
    (do! (var x 1)
         (bitand= x 3)
         (bitxor= x 1)
         (bitor= x 2)
         (bitleft= x 2)
         (bitright= x 1)
         (unbitright= x 1)
         (is (= 2 x))))
  (testing "Attempting to divide by zero throws an ArithmeticException."
    (is (thrown? ArithmeticException (do! (var x 10) (div= x 0))))))

(deftest test-conditionals
  (testing "Conditional 'when!' executes the body when the condition is true."
    (is (= 20 (do! (var x 10)
                   (when! (> x 5)
                     (!= x 20))
                   x))))
  (testing "Conditional 'when-not!' executes the body when the condition is false."
    (is (= 10 (do! (var x 5)
                   (when-not! (< x 5)
                     (!= x 10))
                   x))))
  (testing "Conditional 'when-not!' skips body when condition is false."
    (is (= 5 (do! (var x 5)
                  (when! (> x 5)
                    (!= x 10))
                  x))))
  (testing "Can declare mutable variables inside body of when!"
    (is (= 10 (when! true
                (var x 10)
                x))))
  (testing "Can declare mutable variables inside body of when-not!"
    (is (= 10 (when-not! false
                (var x 10)
                x)))))

(deftest test-procedures
  (testing "Imperative procedures correctly returns a value."
    (is (= 10 ((fn! [] 10))))
    (is (= 10 (do (defn! a-fn [] 10) (a-fn)))))
  (testing "Imperative procedures lets you use local mutable variables."
    (is (= 5 ((fn! [] (var x 0) (+= x 5) x))))
    (is (= 5 (do (defn! a-fn [] (var x 0) (+= x 5) x) (a-fn)))))
  (testing "Imperative procedures lets you return early."
    (is (= 3 ((fn! [] (dotimes [n 10]
                        (when (= n 3)
                          (return n)))))))
    (is (= 3 (do (defn! b-fn [] (dotimes [n 10]
                                  (when (= n 3)
                                    (return n))))
                 (b-fn)))))
  (testing "Imperative procedures lets you return void, which ends up as nil."
    (is (= nil ((fn! [] (return)))))
    (is (= nil (do (defn! c-fn [] (return))
                   (c-fn)))))
  (testing "Imperative procedures return the last result if no (return) call is made."
    (is (= :last ((fn! [] :last))))
    (is (= :last (do (defn! d-fn [] :last)
                     (d-fn))))))

(deftest leetcode
  (testing "167. Two Sum II - Input Array Is Sorted"
    (do (defn! two-sum
          [numbers target]
          (var left 0)
          (var right (dec (count numbers)))
          (while! (< left right)
            (var sum (+ (get numbers left) (get numbers right)))
            (cond
              (= sum target)
              (return [(inc left) (inc right)])
              (< sum target)
              (++ left)
              :else
              (-- right)))
          :no-solution-found)
        (is (= [1 2] (two-sum [2 7 11 15] 9)))
        (is (= [1 3] (two-sum [2 3 4] 6)))
        (is (= [1 2] (two-sum [-1 0] -1))))))
