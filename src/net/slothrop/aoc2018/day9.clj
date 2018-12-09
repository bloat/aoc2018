(ns net.slothrop.aoc2018.day9)

(defn score [next current all player scores size]
  (let [to-remove (mod (- current 7) size)
        removed (aget all to-remove)]
    (System/arraycopy all (inc to-remove) all to-remove (- size to-remove))
    [all
     (update scores player (fnil #(+ next removed %) 0))
     to-remove]))

(defn place-marble [next current all size]
  (let [to-skip (mod (inc current) size)
        next-index (inc to-skip)]
    (System/arraycopy all next-index all (inc next-index) (- size next-index))
    (aset all next-index next)
    [all next-index]))

(defn marble [last-marble number-players]
  (loop [current-marble 0
         current-player 0
         all-marbles (int-array (inc last-marble))
         next-marble 1
         scores {}
         size 1]
;;    (println (take current-marble (into [] all-marbles)) "(" (aget all-marbles current-marble) ")" (drop (inc current-marble) (into [] all-marbles)))
    (when (zero? (mod next-marble 250)) (println next-marble))
    (cond
      (> next-marble last-marble) scores
      (zero? (mod next-marble 23)) (let [[new-marbles new-scores new-current-marble] (score next-marble current-marble all-marbles current-player scores size)]
                                     (recur new-current-marble
                                            (mod (inc current-player) number-players)
                                            new-marbles
                                            (inc next-marble)
                                            new-scores
                                            (dec size)))
      :else (let [[new-marbles new-current-marble] (place-marble next-marble current-marble all-marbles size)]
              (recur new-current-marble
                     (mod (inc current-player) number-players)
                     new-marbles
                     (inc next-marble)
                     scores
                     (inc size))))))

(comment
  ;; part 1
  (apply max (vals (marble 25 9)))
  (apply max (vals (marble 1618 10)))
  (apply max (vals (marble 7999 13)))
  (apply max (vals (marble 1104 17)))
  (apply max (vals (marble 6111 21)))
  (apply max (vals (marble 5807 30)))
  (apply max (vals (marble 72026 471)))
  
  ;; part 2
  (apply max (vals (marble 7202600 471)))
  
  )
