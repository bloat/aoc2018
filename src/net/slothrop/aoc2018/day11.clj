(ns net.slothrop.aoc2018.day11)

(defn power [^long x ^long y ^long serial]
  (let [rack-id (+ x 10)]
    (->
     rack-id
     (* y)
     (+ serial)
     (* rack-id)
     (mod 1000)
     (unchecked-divide-int 100)
     (- 5))))

;; part 1

(defn all-3x3 []
  (for [^long tlx (range 1 299) ^long tly (range 1 299)]
    (vector tlx tly (for [x (range tlx (+ tlx 3)) y (range tly (+ tly 3))]
                      [x y]))))

(defn square->total-power [[tlx tly square] serial]
  (vector tlx tly (apply + (map (fn [[x y]] (power x y serial)) square))))

;; part 2
(defn all-powers [serial]
  (into {}
        (for [x (range 1 301) y (range 1 301)]
          [[x y] (power x y serial)])))

(defn square-from-square-left [[^long tlx ^long tly ^long size ^long power] ap]
  [(inc tlx) tly size
   (let [intermediate-power (loop [y tly ^long sum power]
                              (if (= y (+ tly size))
                                sum
                                (recur (inc y) (- sum (long (ap [tlx y]))))))]
     (loop [y tly ^long sum intermediate-power]
       (if (= y (+ tly size))
         sum
         (recur (inc y) (+ sum (long (ap [(+ tlx size) y])))))))])

(defn square-from-square-above [[^long tlx ^long tly ^long size power] ap]
  [tlx (inc tly) size
   (let [intermediate-power (loop [x tlx ^long sum power]
                              (if (= x (+ tlx size))
                                sum
                                (recur (inc x) (- sum (long (ap [x tly]))))))]
     (loop [x tlx ^long sum intermediate-power]
       (if (= x (+ tlx size))
         sum
         (recur (inc x) (+ sum (long (ap [x (+ tly size)])))))))])

(defn top-left [^long n]
  (vector 1 1 n (for [x (range 1 (inc n)) y (range 1 (inc n))] [x y])))

(defn square->total-power-part2 [[tlx tly n square] all-powers]
  (vector tlx tly n (apply + (map all-powers square))))

(defn best-in-column [[_ _ ^long size _ :as top-of-column] ap]
  [top-of-column (apply max-key #(nth % 3) (take (- 301 size) (iterate #(square-from-square-above % ap) top-of-column)))])

(defn max-for-square-size [^long square-size ap]
  (println square-size)
  (let [top-left (square->total-power-part2 (top-left square-size) ap)]
    (second (apply max-key #(nth (second %) 3)
                   (take (- 301 square-size)
                         (iterate (fn [[top-of-column best]] (best-in-column (square-from-square-left top-of-column ap) ap))
                                  (best-in-column top-left ap)))))))

(comment
  (set! *unchecked-math* true)
  (set! *warn-on-reflection* true)

  ;; part 1
  (apply max-key #(nth % 2) (map #(square->total-power % 5535) (all-3x3)))

  ;; part 2
  (square->total-power (nth (all-3x3) 2) 5535)

  (let [ap (all-powers 5535)]
    (square-from-square-above (square-from-square-above (square->total-power-part2 (top-left 3) ap) ap) ap))

  (let [ap (all-powers 5535)]
    (apply max-key #(nth % 3)
           (map (fn [size] (max-for-square-size size ap)) (range 3 4))))

  (let [ap (all-powers 5535)]
    (apply max-key #(nth % 3)
           (map (fn [size] (max-for-square-size size ap)) (range 1 300))))

  (square->total-power-part2 (top-left 3))

  )
