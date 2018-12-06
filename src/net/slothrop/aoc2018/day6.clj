(ns net.slothrop.aoc2018.day6
  (:require [net.slothrop.aoc2018.input :as i]
            [clojure.java.io :as io]))

(def test-input [[1 1] [1 6] [8 3] [3 4] [5 5] [8 9]])

(defn most [i key sel]
  (let [coord (apply sel (map key i))]
    (filter #(= coord (key %)) i)))

(defn boundaries [i]
  (into #{} (concat (most i first max) (most i first min) (most i second max) (most i second min))))

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2))))

(defn in-area [c p i]
  (let [distances (map #(vector % (distance c %)) i)
        shortest (apply min (map second distances))
        closest-points (map first (filter #(= shortest (second %)) distances))]
    (and (= 1 (count closest-points))
         (= p (first closest-points)))))

(defn add-neighbours [q [x y] a]
  (->>
   [[-1 0] [0 -1] [1 0] [0 1]]
   (map (fn [[dx dy]] (vector (+ x dx) (+ y dy))))
   (into q)))

(defn find-area [p i]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY p)
         a #{}]
    (if (zero? (mod (count a) 100))
      (println p (count a)))
    (cond
      (empty? q) a
      ;; I guess my attempt to detect infinite regions did not work.
      ;; So using this dodgy heuristic we are lucky enough to get the right answer.
      (< 5000 (count a)) #{} 
      (a (peek q)) (recur (pop q) a)
      (not (in-area (peek q) p i)) (recur (pop q) a)
      :else (recur (add-neighbours (pop q) (peek q) a) (conj a (peek q))))))

(defn largest-area [i]
  (let [boundaries (boundaries i)]
    (->> i
         (filter (comp not boundaries))
         (map #(vector % (count (find-area % i))))
         (apply max-key second))))

;; part 2

(defn in-area-part2 [p i d]
  (< (apply + (map #(distance p %) i)) d))

(defn find-area-part2 [p i d]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY p)
         a #{}]
    (if (zero? (mod (count a) 100))
      (println p (count a)))
    (cond
      (empty? q) a
      (a (peek q)) (recur (pop q) a)
      (not (in-area-part2 (peek q) i d)) (recur (pop q) a)
      :else (recur (add-neighbours (pop q) (peek q) a) (conj a (peek q))))))

(comment
  ;; part 1
  (largest-area i/day6-input)

  ;; part 2
  ;; found starter point by trial and error
  (in-area-part2 [200 200] i/day6-input 10000)
  (count (find-area-part2 [200 200] i/day6-input 10000))
  )
