(ns net.slothrop.aoc2018.day13
  (:require
            [clojure.java.io :as io]))

(def input (.split (slurp (io/resource "day13.txt")) "\n"))
(def test-input (.split (slurp (io/resource "day13-example.txt")) "\n"))

(defn parse-char [c column-number]
  (condp = c
    \/ :curvea
    \\ :curveb
    \- :horiz
    \| :vert
    \+ :junction
    \> :horiz
    \< :horiz
    \^ :vert
    \v :vert
    nil))

(defn parse-track-line [l]
  (into [] (map parse-char l (range))))

(defn parse-tracks [input]
  (into [] (map parse-track-line input)))

(defn track-at-point [x y tracks]
  (get-in tracks [y x]))

(defn find-trains [input]
  (into [] (mapcat
            (fn [line line-number]
              (filter identity (map (fn [c column-number]
                                      (condp = c
                                        \> (vector 1 0 column-number line-number :left)
                                        \< (vector -1 0 column-number line-number :left)
                                        \^ (vector 0 -1 column-number line-number :left)
                                        \v (vector 0 1 column-number line-number :left)
                                        nil)) line (range))))
            input (range))))

(defn new-direction-x [dx dy new-track turn]
  (condp = new-track
   :curveb (if (= dx 0) dy 0)
   :curvea (if (= dx 0) (- dy) 0)
   :junction (condp = turn
               :left (if (= dx 0) dy 0)
               :right (if (= dx 0) (- dy) 0)
               dx)
   dx))

(defn new-direction-y [dx dy new-track turn]
  (condp = new-track
    :curveb (if (= dy 0) dx 0)
    :curvea (if (= dy 0) (- dx) 0)
    :junction (condp = turn
                :left (if (= dy 0) (- dx) 0)
                :right (if (= dy 0) dx 0)
                dy)
    dy))

(defn new-next-turn [new-track turn]
  (if (= :junction new-track)
    (condp = turn
      :left :straight
      :straight :right
      :right :left)
    turn))

(defn find-crash [train trains]
  (let [[_ _ nx ny _] train]
    (some (fn [[_ _ x y _]] (and (= nx x) (= ny y))) trains)))

(defn move-trains [trains tracks]
  (loop [new-trains [] remaining-trains (sort-by (fn [[_ _ x y _]] [y x]) trains)]
    (if (empty? remaining-trains)
      new-trains
      (let [[dx dy x y turn] (first remaining-trains)
            new-position-x (+ x dx)
            new-position-y (+ y dy)
            new-track (track-at-point new-position-x new-position-y tracks)
            new-train (vector (new-direction-x dx dy new-track turn)
                              (new-direction-y dx dy new-track turn)
                              new-position-x
                              new-position-y
                              (new-next-turn new-track turn))]
        (if (or (find-crash new-train new-trains) (find-crash new-train remaining-trains))
          (throw (RuntimeException. (str new-train)))
          (recur (conj new-trains new-train) (rest remaining-trains)))))))

;; part 2
(def test-input2 (.split (slurp (io/resource "day13-example2.txt")) "\n"))

(defn remove-train [x y trains]
  (filter (fn [[_ _ tx ty _]] (not (and (= x tx) (= y ty)))) trains))

(defn move-trains-part2 [trains tracks i]
  (when (zero? (mod i 200)) (println i (count trains)))
  (loop [new-trains [] remaining-trains (sort-by (fn [[_ _ x y _]] [y x]) trains)]
    (if (empty? remaining-trains)
      (if (= (count new-trains) 1)
        (throw (RuntimeException. (str new-trains)))
        [new-trains (inc i)])
      (let [[dx dy x y turn] (first remaining-trains)
            new-position-x (+ x dx)
            new-position-y (+ y dy)
            new-track (track-at-point new-position-x new-position-y tracks)
            new-train (vector (new-direction-x dx dy new-track turn)
                              (new-direction-y dx dy new-track turn)
                              new-position-x
                              new-position-y
                              (new-next-turn new-track turn))]
        (if (find-crash new-train new-trains)
          (recur (remove-train new-position-x new-position-y new-trains) (rest remaining-trains))
          (if (find-crash new-train remaining-trains)
            (recur new-trains (remove-train new-position-x new-position-y (rest remaining-trains)))
            (recur (conj new-trains new-train) (rest remaining-trains))))))))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  ;; part 1
  (track-at-point 12 4 (parse-tracks test-input))
  (parse-track-line (nth test-input 4))
  
  (nth (iterate #(move-trains % (parse-tracks input)) (find-trains input)) 1000)
  (parse-tracks)

  (first (filter identity (map find-crash (iterate #(move-trains % (parse-tracks input)) (find-trains input)))))
  ;; part 2
  (nth (iterate (fn [[trains i]] (move-trains-part2 trains (parse-tracks input) i)) [(find-trains input) 0]) 100000)
  )
