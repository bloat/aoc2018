(ns net.slothrop.aoc2018.day1
  (:require [net.slothrop.aoc2018.input :as i]))

(defn part2 [i]
  (reduce (fn [[seen total] c]
            (let [new-total (+ total c)]
              (if (contains? seen new-total)
                (reduced new-total)
                [(conj seen new-total) new-total]))) [#{} 0] (cycle i)))

(comment
  ;; part1
  (reduce + 0 i/day1-input)

  ;;part2
  (part2 i/day1-input)
)
