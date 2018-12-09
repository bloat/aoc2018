(ns net.slothrop.aoc2018.day9)

(defn score [next current all player scores size]
  (let [to-remove (mod (- current 7) size)
        remaining (drop to-remove all)]
    [(concat (take to-remove all) (rest remaining))
     (update scores player (fnil #(+ next (first remaining) %) 0))
     to-remove]))

(defn place-marble [next current all size]
  (let [to-skip (mod (inc current) size)
        next-index (inc to-skip)]
    [(concat (take next-index all) (conj (drop next-index all) next)) next-index]))

(defn marble [last-marble number-players]
  (loop [current-marble 0
         current-player 0
         all-marbles [0]
         next-marble 1
         scores {}
         size 1]
;;   (println (take current-marble all-marbles) "(" (first (drop current-marble all-marbles)) ")" (drop (inc current-marble) all-marbles))
;;    (when (zero? (mod next-marble 250)) (println next-marble))
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
  
  )
