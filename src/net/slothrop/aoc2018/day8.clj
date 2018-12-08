(ns net.slothrop.aoc2018.day8
  (:require [net.slothrop.aoc2018.input :as i]
            [clojure.java.io :as io]))

(def input (into [] (map #(Integer/parseInt %) (.split (slurp (io/resource "day8.txt")) "\\s"))))
(def test-input [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2] )

(declare read-nodes)

(defn read-metadata [number-metadata metadata-start input]
  (let [end (+ number-metadata metadata-start)]
    [(subvec input metadata-start end) end]))

(defn read-node [start-position input]
  (let [number-child-nodes (nth input start-position)
        number-metadata (nth input (inc start-position))
        [nodes metadata-start] (read-nodes number-child-nodes (+ 2 start-position) input)
        [metadata next-node-start] (read-metadata number-metadata metadata-start input)]
    [next-node-start {:nodes nodes :metadata metadata}]))

(defn read-nodes [number-nodes start-position input]
  (loop [current-node-start-position start-position i 0 nodes []]
    (if (= i number-nodes)
      [nodes current-node-start-position]
      (let [[next-node-start node] (read-node current-node-start-position input)]
        (recur next-node-start (inc i) (conj nodes node))))))

(defn sum-meta-data [node]
  (->> node
       (tree-seq map? :nodes)
       (map :metadata)
       flatten
       (apply +)))

;; part 2
(defn sum-node [node]
  (cond
    (nil? node) 0
    (empty? (:nodes node)) (apply + (:metadata node))
    :else (apply + (map #(sum-node (get-in node [:nodes (dec %)])) (:metadata node)))))

(comment
  ;; part 1
  (sum-meta-data (second (read-node 0 input)))
  ;; part 2
  (sum-node (second (read-node 0 input)))
  () (second (read-node 0 test-input))
  )
