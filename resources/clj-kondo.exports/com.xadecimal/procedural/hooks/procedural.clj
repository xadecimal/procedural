(ns hooks.procedural
  (:require [clj-kondo.hooks-api :as api]))

(declare var-scope*)
(declare for!)

(defn transform
  [children]
  (let [inners #(when (symbol? %)
                  (or (= "var-scope" (name %))
                      (= "do!" (name %))))
        inner-fors #(when (symbol? %)
                      (= "for!" (name %)))]
    (loop [result []
           remaining children]
      (if (empty? remaining)
        result
        (let [[node & tail] remaining]
          (cond
            (and (api/list-node? node)
                 (= 3 (count (:children node)))
                 (= (api/sexpr (first (:children node))) 'var))
            (let [name (second (:children node))
                  value (last (:children node))]
              (recur (conj result (with-meta
                                    (api/list-node
                                     (list*
                                      (api/coerce `let)
                                      (api/vector-node [name value])
                                      (transform tail)))
                                    (meta node)))
                     []))
            (and (api/list-node? node)
                 (inners (api/sexpr (first (:children node)))))
            (recur (conj result (var-scope* (:children node))) tail)
            (and (api/list-node? node)
                 (inner-fors (api/sexpr (first (:children node)))))
            (recur (conj result (:node (for! {:node node}))) tail)
            (api/list-node? node)
            (recur (conj result (with-meta
                                  (api/list-node (transform (:children node)))
                                  (meta node))) tail)
            (and (api/token-node? node)
                 (or (inners (api/sexpr node))
                     (inner-fors (api/sexpr node))))
            (recur result tail)
            :else
            (recur (conj result node) tail)))))))

(defn var-scope*
  [children]
  (api/list-node (cons (api/coerce 'do) (transform children))))

(defn do! [{:keys [node]}]
  (let [children (:children node)
        new-node (var-scope* children)]
    {:node new-node}))

(defn fn! [{:keys [node]}]
  (let [children (:children node)
        new-node (api/list-node (cons (with-meta
                                        (api/coerce `fn)
                                        (meta (first children)))
                                      (transform (rest children))))]
    {:node new-node}))

(defn defn! [{:keys [node]}]
  (let [children (:children node)
        new-node (api/list-node (cons (with-meta
                                        (api/coerce `defn)
                                        (meta (first children)))
                                      (transform (rest children))))]
    {:node new-node}))

(defn for! [{:keys [node]}]
  (let [[for-sym init condition mutation & children] (:children node)
        new-node (api/list-node
                  `(~(api/coerce `try)
                    ~(do!
                      {:node
                       (api/list-node
                        `(~(api/coerce `com.xadecimal.procedural/do!)
                          ~@(if (api/vector-node? init)
                              (:children init)
                              `(~init))
                          ~(api/list-node
                            `(~(api/coerce `while) ~condition
                              ~(api/list-node
                                `(~(api/coerce `try)
                                  ~@(transform children)
                                  ~(api/list-node
                                    `(~(api/coerce `catch)
                                      ~(api/coerce 'com.xadecimal.procedural.ExContinue)
                                      ~(api/coerce '_)))
                                  ~(api/list-node
                                    `(~(api/coerce `finally)
                                      ~@(if (api/vector-node? mutation)
                                          (:children mutation)
                                          `(~mutation))))))))))})
                    ~(api/list-node
                      `(~(api/coerce `catch)
                        ~(api/coerce 'com.xadecimal.procedural.ExBreak)
                        ~(api/coerce '_)))))]
    {:node new-node}))

#_(hooks.procedural/for! {:node (api/parse-string "(p/for! (var i 0) (< i 5) (++ i)
        (p/for! (var j 0) (< j i) (++ j)
          (swap! result + j)))")})

#_(do (require '[clj-kondo.core :as clj-kondo])
      (def code "(require '[com.xadecimal.procedural :as p]) (p/for! (var i 0) (< i 5) (p/++ i)
        (p/for! (var j 0) (< j i) (p/++ j)
          (swap! result + j)))")
      (binding [api/*reload* true]
        (:findings (with-in-str code (clj-kondo/run! {:lint ["-"]})))))
