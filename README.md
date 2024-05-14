# Procedural

Procedural is a Clojure library designed to integrate classic procedural programming features into the functional world of Clojure. This library brings familiar constructs such as for-loops, mutable local variables, function-scoped variables, and early return capabilities. It was built for practicing coding problems such as those from LeetCode, in Clojure.

## Foreword

This library is primarily intended for use in practicing algorithm and data structure problems, such as those found on coding challenge sites like LeetCode. It allows Clojure users to explore procedural and imperative solutions along with the typical functional and immutable approaches native to Clojure. **This library is not recommended for general Clojure application development**, as it does not promote the idiomatic use of Clojure's functional and immutable paradigms. Please ensure you understand the implications of introducing mutable state and imperative logic into your Clojure projects.

## Features

- **Mutable Local Variables**: Declare and mutate local variables within a specific scope.
- **For Loops**: Use traditional for-loop syntax for iterative operations.
- **While Loops and Do-While Loops**: Implement standard while and do-while loops.
- **Early Return**: Exit from functions at any point with an early return.
- **Function Scoped Variables**: Variables that are scoped within the function they are declared in.

## Installation

To add Procedural to your Clojure project, add the dependency to your project configuration file depending on the build tool you are using:

### Leiningen
Add the following to your `project.clj` file:
```clj
[com.xadecimal/procedural "0.1.0"]
```

### Clojure CLI/deps.edn
Add the following to your `deps.edn`:
```clj
{:deps {com.xadecimal/procedural {:mvn/version "0.1.0"}}}
```

## Usage

### Requiring

Everything you need is in the `com.xadecimal.procedural` namespace. Using `refer :all` is convenient, but at your preference you can also refer each macro/function explicitly.

```clojure
(ns <your-ns>
  (:require [com.xadecimal.procedural :refer :all]))
```

### Declaring Mutable Variables

Use the `do!` macro to declare and manipulate variables within a local scope.

```clojure
(do!
  (var x 10)
  (!= x 20) ; Set x to 20
  (println x)) ; Prints 20
```

### For Loops

The `for!` loop allows for traditional imperative looping. When initializing or mutating only one variable, the syntax is straightforward:

```clojure
(for! (var i 0) (< i 10) (++ i)
  (println i))
```

For complex scenarios involving multiple variables, use vectors for initialization and mutation:

```clojure
(for! [(var i 0) (var j 10)] (and (< i 10) (> j 0)) [(++ i) (-- j)]
  (println i j))
```

To control loop execution, `for!` supports `continue` and `break`:

```clojure
(for! (var i 0) (< i 10) (++ i)
  (when (= i 5) (continue)) ; Skip the rest of the loop when i is 5
  (when (= i 8) (break))    ; Exit the loop when i is 8
  (println i))
```

Similar to how you can omit parts of a for loop declaration in procedural languages, typically using a semicolon (;) to represent an empty section, you can skip parts of `for!` using `_`.

```clojure
(require '[com.xadecimal.procedural :refer [for! _ ++]])

(for! (var i 0) (< i 10) _
  (println i)
  (++ i))
```

### While Loops

`while!` loops execute as long as the condition is true:

```clojure
(var i 0)
(while! (< i 10)
  (++ i)
  (println i))
```

For fine-grained control, they support `continue` and `break`:

```clojure
(var i 0)
(while! (< i 10)
  (++ i)
  (when (= i 4) (continue)) ; Skip the current iteration when i is 4
  (when (= i 7) (break))    ; Exit the loop when i is 7
  (println i))
```

### Do-While Loops

`do-while!` loops ensure that the loop body executes at least once:

```clojure
(var i 0)
(do-while! (< i 5)
  (++ i)
  (println i))
```

They also support `continue` and `break` for loop control:

```clojure
(var i 0)
(do-while! (< i 5)
  (++ i)
  (when (= i 2) (continue)) ; Skip the current iteration when i is 2
  (when (= i 4) (break))    ; Exit the loop when i is 4
  (println i))
```

### Assignment Operators

The following table lists all the assignment operators provided by the Procedural library:

| Operator   | Description                                      |
|------------|--------------------------------------------------|
| `!=`       | Sets the variable to a new value.                |
| `+=`       | Adds and assigns the result.                     |
| `-=`       | Subtracts and assigns the result.                |
| `*=`       | Multiplies and assigns the result.               |
| `/=`       | Divides and assigns the result.                  |
| `quot=`    | Divides and assigns the integer quotient.        |
| `rem=`     | Divides and assigns the remainder.               |
| `bitand=`  | Performs a bitwise AND and assigns the result.   |
| `bitor=`   | Performs a bitwise OR and assigns the result.    |
| `bitxor=`  | Performs a bitwise XOR and assigns the result.   |
| `bitleft=` | Left shifts and assigns the result.              |
| `bitright=`| Right shifts and assigns the result.             |
| `unbitright=` | Unsigned right shifts and assigns the result. |

### Conditionals with `when!` and `when-not!`

`when!` and `when-not!` are enhanced versions of Clojure's `when` and `when-not` that allow the use of mutable variables within their bodies:

```clojure
(var x 5)
(when! (> x 3)
  (var x 0)
  (!= x 10)
  (println x)) ; Prints 10
(when-not! (> x 10)
  (var x 0)
  (!= x 20)
  (println x)) ; Prints 20
(println x) ; Prints 5
```

You can also use `do!` with Clojure’s standard conditional constructs to achieve imperative-style conditionals that allow local variable mutations:

```clojure
;; Using `if`
(if (> 10 5)
  (do! (var i 0)
       (+= i 10)
       i)
  (do! (var i 10)
       (-- i)
       i))

;; Using `cond`
(cond
  (> 10 20) (do! (var result 0) (+= result 10) result)
  :else     (do! (var result 10) (-- result) result))

;; Using `case`
(case 2
  1 (do! (var x 10) (++ x) x)
  2 (do! (var x 20) (-- x) x)
  (do! (var x 0) x))
```

### Defining Procedures with `defn!` and `fn!`

Use `defn!` to define named procedures and `fn!` for anonymous procedures. Both allow for local mutable variables and early returns within the procedure scope.

```clojure
;; Named procedure
(defn! my-procedure [a b]
  (var result (+ a b))
  (when (> result 10) (return "High"))
  "Low")
(my-procedure 15 10) ; Returns "High"

;; Anonymous procedure
(is (= "High" ((fn! [x y]
                 (var sum (+ x y))
                 (if (> sum 20) (return "High") "Low")) 15 10)))
```

### Early Return

Procedural allows you to exit functions prematurely using `return`.

```clojure
(defn! my-function []
  (var x 100)
  (if (> x 50)
    (return "High")
    (return "Low")))
```
