(ns net.slothrop.aoc2018.day17-2
  (:require
   [clojure.java.io :as io]
   [quil.core :as q]))

(def input (.split (slurp (io/resource "day17.txt")) "\n"))
(def test-input (.split "x=495, y=2..7
y=7, x=495..501
x=501, y=3..7
x=498, y=2..4
x=506, y=1..2
x=498, y=10..13
x=504, y=10..13
y=13, x=498..504" "\n"))

(defn parse-line [l]
  (let [matcher (re-matcher #"([x|y])=(\d+), ([x|y])=(\d+)\.\.(\d+)" l)]
    (when (re-find matcher)
      (let [[_ ax1 coord1 ax2 coord2a coord2b] (re-groups matcher)
            coord1 (Integer/parseInt coord1)]
        (for [coord2 (range (Integer/parseInt coord2a) (inc (Integer/parseInt coord2b)))]
          (if (= "x" ax1)
            [coord1 coord2]
            [coord2 coord1]))))))

(defn parse [i]
  (into #{} (mapcat parse-line i)))

(def clay-atom (atom #{}))
(def all-water (atom #{}))
(def settled-water-atom (atom #{}))

(defn hit-wall [w clay]
  (clay w))

(defn hit-water [w water]
  (water w))

(defn over-the-edge [[x y :as w] clay settled-water]
  (let [below [x (inc y)]]
    (not (or (clay below) (settled-water below)))))

(defn end-row [w clay settled-water] 
  (or (clay w) (over-the-edge w clay settled-water)))

(defn line-down [[x y] clay water max-y]
  (let [[new-water [next & _ :as rest]] (split-with #(and (not (hit-wall % clay))
                                                          (not (hit-water % water))) (map (partial vector x) (range y (inc max-y))))]
    (if (or (nil? rest)
            (nil? next)
            (some? (second (line-right next clay water)))
            (some? (nth (line-left next clay water) 2)))
      [new-water nil]
      [new-water (last new-water)])))

(defn line-right [[x y :as w] clay settled-water]
  (let [[water [next & _]] (split-with #(do (println %) (not (end-row % clay settled-water))) (map #(vector % y) (iterate inc x)))]
    (if (hit-wall next clay)
      [water nil]
      [water next])))

(defn line-left [[x y :as w] clay settled-water]
  (let [[water [next & _]] (split-with #(not (end-row % clay settled-water)) (map #(vector % y) (iterate dec x)))]
    [water next]
    (if (hit-wall next clay)
      [water [x (dec y)] nil]
      [water nil next])))

(defn flow [spout clay]
  (reset! clay-atom clay)
  (let [max-y (apply max (map second clay))
        min-y (apply min (map second clay))]
    (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) spout) water #{} settled-water #{} row-start nil]
      (reset! all-water water)
      (reset! settled-water-atom settled-water)
      (if row-start
        (let [[nw-lr new-spout-lr] (line-right row-start clay settled-water)
              [nw-ll new-rs new-spout-ll] (line-left row-start clay settled-water)]
          (if (and new-rs (not new-spout-lr))
            (recur q (into (into water nw-lr) nw-ll) (into (into settled-water nw-lr) nw-ll) new-rs)
            (let [nq (if new-spout-lr (conj q new-spout-lr) q)
                  nq (if new-spout-ll (conj nq new-spout-ll) nq)]
              (recur nq (into (into water nw-lr) nw-ll) settled-water nil))))
        (if (empty? q)
          (count (filter (fn [[x y]] (<= min-y y max-y)) settled-water))
          (let [[new-water row-start] (line-down (peek q) clay water max-y)]
            (recur (pop q) (into water new-water) settled-water row-start)))))))

;; vis

(defn quil-setup []
  (q/frame-rate 10)
  (q/background 200))


(def dy (atom 0))

(defn quil-draw []
  (when (q/key-pressed?)
    (swap! dy + 200))
  (q/clear)
  (q/background 200)
  (q/text (str (dec (count @all-water))) 200 200)
  (q/no-fill)
  (doseq [[x y] @clay-atom]
    (q/rect (- (* 5 x) 1850) (-  (* 6 y) @dy) 5 6))
  (q/fill 0 0 130)
  (doseq [[x y] @all-water]
    (q/rect (- (* 5 x) 1850) (-  (* 6 y) @dy) 5 6))
  (q/fill 255 0 0)
  (doseq [[x y] @settled-water-atom]
    (q/rect (- (* 5 x) 1850) (-  (* 6 y) @dy) 5 6))
)

(q/defsketch example                  ;; Define a new sketch named example
  :title "Oh so many grey circles"    ;; Set the title of the sketch
  :settings #(q/smooth 2)             ;; Turn on anti-aliasing
  :setup quil-setup                        ;; Specify the setup fn
  :draw quil-draw                          ;; Specify the draw fn
  :size [1323 1000])                    ;; You struggle to beat the golden ratio

;; part2

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  ;; part 1
  (def ex-clay (parse test-input))
  (def clay (parse input))
  (line-left [500 6] ex-clay #{})
  (line-down [500 0] ex-clay #{} 13)
  (ex-clay [500 7])
  (flow [500 0] clay)

  (apply min (map second clay))
  367789
  
  ;; part 2


  )
