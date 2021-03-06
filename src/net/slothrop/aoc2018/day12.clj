(ns net.slothrop.aoc2018.day12
  (:require
            [clojure.java.io :as io]))

(def rule (into {}
                (map (fn [line]
                       [(take 5 line) (last line)])
                     (.split (slurp (io/resource "day12.txt")) "\n"))))

(def input "###....#..#..#......####.#..##..#..###......##.##..#...#.##.###.##.###.....#.###..#.#.##.#..#.#")

(def start-row [(repeat \.) (lazy-cat (seq input) (repeat \.))])

(def max-left 50)
(def max-right (+ (count input) 50))

(defn evolve-lhs [[lhs rhs]]
  (map (comp rule reverse) (partition 5 1 (cons (second rhs) (cons (first rhs) lhs)))))

(defn evolve-rhs [[lhs rhs]]
  (map rule (partition 5 1 (cons (second lhs) (cons (first lhs) rhs)))))

(defn evolve-row [row]
  [(evolve-lhs row) (evolve-rhs row)])


(defn score [row]
  (reduce (fn [a [v s]] (if (= \# v)
                          (+ a s)
                          a)) 0 (map vector (take 5000 (second row)) (range))))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  (let [twentieth (nth (iterate evolve-row start-row) 5000)]
    (reduce (fn [a [v s]] (if (= \# v)
                            (+ a s)
                            a)) 0 (map vector (take max-right (second twentieth)) (range))))
  ;; part 1

  ;; part 2
  ;; Not much extra code - found the cycle by looking at Mathematica's CellularAutomaton output. Then 
  ;; fiddled around here to get the start point of the cycle and the score increment each time
  (take 50 (drop 180 (map vector (map score (iterate evolve-row start-row)) (range))))
  (+ 6505 (* 34 (- 50000000000 191)))

  )
