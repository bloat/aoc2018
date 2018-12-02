(ns net.slothrop.aoc2018.day2
  (:require [net.slothrop.aoc2018.input :as i]))

(defn has-n [n s]
  (some #(= n %) (vals (frequencies s))))

(defn count-with [n i]
  (count (filter #(has-n n %) i)))

(def l (count (first i/day2-input)))

(defn remove-nth [n s]
  (let [letters (into [] s)]
    [(subvec letters 0 n) (subvec letters (inc n) l)]))

(defn remove-each [n i]
  (map #(remove-nth n %) i))

(defn has-answer [candidates]
  (some (fn [[result count]] (when (= count 2) result)) (frequencies candidates)))

(comment
  ;; part 1
  (* (count-with 2 i/day2-input) (count-with 3 i/day2-input))

  ;; part 2
  (apply str (apply concat (some has-answer (map #(remove-each % i/day2-input) (range 0 26)))))

  )
