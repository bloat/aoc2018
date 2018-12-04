(ns net.slothrop.aoc2018.day4
  (:require [net.slothrop.aoc2018.input :as i]
            [clojure.java.io :as io])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.time.temporal ChronoUnit]))

(def input (into [] (.split (slurp (io/resource "day4.txt")) "\n")))
(def test-input (into [] (.split (slurp (io/resource "day4-example.txt")) "\n")))

(def parser (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn input-time [input]
  (LocalDateTime/from (.parse parser (subs input 1 17))))

(def sorted-input (sort-by input-time input))

(defn minutes-between [l1 l2]
  (.until l1 l2 ChronoUnit/MINUTES))

(defn count-sleeping [[current-id fell-asleep counts] line]
  (case (.charAt line 19)
    \G [(subs line 26 (.indexOf line (int \space) 26)) nil counts]
    \f [current-id (input-time line) counts]
    \w [current-id nil (update-in counts [current-id] (fnil + 0) (minutes-between fell-asleep (input-time line)))]))

(defn largest-val [m]
  (->>
   m
   (sort-by val)
   last
   key))

(def sleepy-guard (largest-val (nth (reduce count-sleeping [nil nil {}] sorted-input) 2)))

(defn count-minutes [[current-id fell-asleep counts] line]
  (case (.charAt line 19)
    \G [(subs line 26 (.indexOf line (int \space) 26)) nil counts]
    \f [current-id (Integer/parseInt (subs line 15 17)) counts]
    \w (if (= current-id sleepy-guard)
         [current-id nil (reduce (fn [c l] (update-in c [l] (fnil inc 0))) counts (range fell-asleep (Integer/parseInt (subs line 15 17))))]
         [current-id nil counts])))

(defn count-guard-minutes [[current-id fell-asleep counts] line]
  (case (.charAt line 19)
    \G [(subs line 26 (.indexOf line (int \space) 26)) nil counts]
    \f [current-id (Integer/parseInt (subs line 15 17)) counts]
    \w [current-id nil (reduce (fn [c l] (update-in c [current-id l] (fnil inc 0))) counts (range fell-asleep (Integer/parseInt (subs line 15 17))))]))

(comment
  ;; part 1
  (* (Integer/parseInt sleepy-guard)
     (largest-val (nth (reduce count-minutes [nil nil {}] sorted-input) 2)))

  ;; part 2
  (let [[g [m _]]
        (last
         (sort-by
          (comp second val)
          (into {}
                (map
                 (fn [[guard counts]]
                   [guard (last (sort-by val counts))])
                 (nth (reduce count-guard-minutes [nil nil {}] sorted-input) 2)))))]
    (* m (Integer/parseInt g)))
  )
