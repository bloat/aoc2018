(ns net.slothrop.aoc2018.day3
  (:require [net.slothrop.aoc2018.input :as i]
            [clojure.java.io :as io]))

(def input (into [] (.split (slurp (io/resource "day3.txt")) "\n")))
(def test-input ["#1 @ 1,3: 4x4" "#2 @ 3,1: 4x4" "#3 @ 5,5: 2x2"])

(defn empty-fabric [size] (into [] (repeat size (into [] (repeat size 0)))))

(defn parse-patch [patch]
  (let [matcher (re-matcher #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" patch)]
    (when (re-find matcher)
      (map #(Integer/parseInt %) (drop 1 (re-groups matcher))))))

(defn apply-patch [fabric [_ left top width height]]
  (reduce
   #(update-in %1 %2 inc)
   fabric
   (for [x (range left (+ left width)) y (range top (+ top height))] [x y])))

(defn apply-patches [fabric input applier]
  (reduce
   applier
   fabric
   (map parse-patch input)))

(defn count-overlap [fabric]
  (count (filter #(> % 1) (flatten fabric))))

(defn empty-fabric-part2 [size] (into [] (repeat size (into [] (repeat size #{})))))

(defn apply-patch-part2 [fabric [id left top width height]]
  (reduce
   #(update-in %1 %2 conj id)
   fabric
   (for [x (range left (+ left width)) y (range top (+ top height))] [x y])))

(defn check-patch [flat-fabric id]
  (when (zero? (mod id 10))
    (println id))
  (empty? (filter #(> (count %) 1) (filter #(contains? % id) flat-fabric))))

(defn check-patches [flat-fabric input]
  (filter #(check-patch flat-fabric %) (map #(first (parse-patch %)) input)))

(comment
  ;; part 1
  (count-overlap (apply-patches (empty-fabric 1000) input apply-patch))


  ;; part 2
  
  (check-patches (flatten (apply-patches (empty-fabric-part2 1000) input apply-patch-part2)) input)
  )
