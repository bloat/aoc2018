(ns net.slothrop.aoc2018.day7
  (:require
            [clojure.java.io :as io]))

(def input (into [] (.split (.trim (slurp (io/resource "day7.txt"))) "\n")))
(def test-input (into [] (.split "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin." "\n")))

(defn parse-line [i]
  (vector (.charAt i 5) (.charAt i 36)))

(defn parse [i]
  (into (sorted-map) (reduce (fn [r [in node]] (merge {in #{}} (update-in r [node] (fnil conj #{}) in))) {} (map parse-line i))))

(defn update-remaining [r n]
  (dissoc (into (sorted-map) (map (fn [[k v]] [k (disj v n)]) r)) n))

(defn next-ready [r] (ffirst (filter #(zero? (count (second %))) r)))

(defn top-sort [i]
  (loop [remaining i s []]
    (if (empty? remaining)
      s
      (let [next (next-ready remaining)]
        (recur (update-remaining remaining next) (conj s next))))))

;; part 2

(defn next-finish [w]
  (let [[worker [time item]] (apply min-key (comp first second) w)]
    [worker time item]))

(defn time-sort [i n offset]
  (loop [remaining i t 0 w {} s []]
    (if (and (empty? remaining) (empty? w))
      [s t w]
      (let [next (next-ready remaining)]
        (if (or (nil? next) (= n (count w)))
          (let [[next-to-finish time-to-finish item-to-finish] (next-finish w)]
            (recur (update-remaining remaining item-to-finish) time-to-finish (dissoc w next-to-finish) (conj s item-to-finish)))
          (recur (dissoc remaining next) t (assoc w (first (filter #(not (contains? w %)) (range 0 n))) [(+ t (int next) offset) next])  s))))))

(comment
  ;; part 1
  (parse-line (first input))
  (apply str (top-sort (parse input)))
  ;; part 2
  (time-sort (parse test-input) 2 -64)
  (time-sort (parse input) 5 -4)
  
)
