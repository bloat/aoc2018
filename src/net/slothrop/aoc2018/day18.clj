(ns net.slothrop.aoc2018.day18
  (:require
   [clojure.java.io :as io]))

(def input (into [] (map #(into [] %) (.split (slurp (io/resource "day18.txt")) "\n"))))
(def test-input (into [] (map #(into [] %) (.split (slurp (io/resource "day18-example.txt")) "\n"))))

(def deltas (for [dx [-1 0 1] dy [-1 0 1] :when (not (and (zero? dx) (zero? dy)))] [dy dx]))

(defn update-acre [forest [y x :as pos]]
  (let [max-y (count forest)
        max-x (count (first forest))]
    (let [neighbours
          (map #(get-in forest %)
               (filter (fn [[y x]] (and (< -1 x max-x) (< -1 y max-y)))
                       (for [[dy dx] deltas] [(+ y dy) (+ x dx)])))]
      (condp = (get-in forest pos)
        \. (if (<= 3 (count (filter #(= \| %) neighbours))) \| \.)
        \| (if (<= 3 (count (filter #(= \# %) neighbours))) \# \|)
        \# (let [distinct-neighbours (into #{} neighbours)]
             (if (and (contains? distinct-neighbours \#)
                      (contains? distinct-neighbours \|))
               \#
               \.))))))

(defn update-forest-row [number forest]
  (into [] (map #(update-acre forest [number %]) (range 0 (count (first forest))))))

(defn update-forest [forest]
  (into [] (map #(update-forest-row % forest) (range 0 (count forest)))))

(defn score [forest]
  (let [all-acres (flatten forest)]
    (* (count (filter #(= \| %) all-acres))
       (count (filter #(= \# %) all-acres)))))

;; part2

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  ;; part 1
  (take 1000 (map score (iterate update-forest input)))
  
  
  ;; part 2

  (- 1000000000 (+ 636 (* 35714263 28)))

  )
