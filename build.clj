(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'com.xadecimal/procedural)
(def version "0.0.0")

(def class-dir "target/classes")
(def basis (b/create-basis))

(defn clean [_]
  (b/delete {:path "target"}))

(defn compile [_]
  (b/javac {:src-dirs ["jsrc"]
            :class-dir class-dir
            :basis basis
            :javac-opts ["-source" "8" "-target" "8"]}))
