(ns net.slothrop.aoc2018.day5
  (:require
            [clojure.java.io :as io]))

(def input (.trim (slurp (io/resource "day5.txt"))))
(def test-input "dabAcCaCBAcCcaDA")

(defn match [c1 c2]
  (or
   (and (Character/isUpperCase c1) (Character/isLowerCase c2) (= c1 (Character/toUpperCase c2)))
   (and (Character/isUpperCase c2) (Character/isLowerCase c1) (= c2 (Character/toUpperCase c1)))))

(defn react [^StringBuilder r]
  (loop [i 0]
    (cond
      (>= i (dec (.length r))) (str r)
      (match (.charAt r i) (.charAt r (inc i))) (recur (do (.delete r i (+ 2 i)) (max 0 (dec i))))
      :else (recur (inc i)))))

(defn remove-all [^StringBuilder s c]
  (let [uc (Character/toUpperCase c)]
    (loop [i 0]
      (cond
        (= i (.length s)) s
        (or (= c (.charAt s i)) (= uc (.charAt s i))) (recur (do (.deleteCharAt s i) i))
        :else (recur (inc i))))))

(comment
  ;; part 1
  (count (react (StringBuilder. input)))

  ;; part 2
  (apply min (map #(count (react (remove-all (StringBuilder. input) %))) "abcdefghijklmnopqrstuvwxyz"))
  )
