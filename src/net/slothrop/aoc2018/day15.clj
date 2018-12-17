(ns net.slothrop.aoc2018.day15)

(defn make-cave [^String s]
  (into {} (mapcat (fn [l y] (map (fn [c x] [[x y] (if (= c \#) c \.)]) l (range)))
                   (.split s "\n") (range))))

(defn make-units [^String s]
  (into [] (filter (comp some? second) (mapcat (fn [l y] (map (fn [c x] (cond
                                                                          (= c \E) [x y 200 :elf]
                                                                          (= c \G) [x y 200 :goblin])) l (range)))
                                               (.split s "\n") (range)))))

(defn reading-order [units]
  (sort-by (fn [[x y _ _]] [y x]) units))

(defn reading-hp-order [units]
  (sort-by (fn [[x y hp _]] [hp y x]) units))

(defn identify-targets [i units]
  (let [[_ _ _ type] (units i)]
    (filter (fn [[_ _ _ othertype :as other]] (and (not (nil? other)) (not (= type othertype)))) units)))

(defn final-score [n units]
  (* (dec n) (reduce (fn [acc [_ _ hp _]] (+ acc hp)) 0 (filter identity units))))

(def deltas [[0 -1] [-1 0] [1 0] [0 1]]) ;; in reading order

(defn can-attack [unit targets]
  (let [[unitx unity _ _] unit]
    (first (reading-hp-order (filter (fn [[^long targetx ^long targety _ _]]
                                       (some
                                        (fn [[adjx adjy]] (and (= adjx unitx) (= adjy unity)))
                                        (map (fn [[^long dx ^long dy]] [(+ targetx dx) (+ targety dy)])
                                             deltas)))
                                     targets)))))

(defn attack [target units]
  (let [[tx ty _ _] target]
    (into []
          (map
           (fn [[ux uy ^long hp type :as unit]] (if (and (= ux tx) (= uy ty))
                                                  (if (> hp 3)
                                                    [ux uy (- hp 3) type]
                                                    nil)
                                                  unit))
           units))))

(defn new-paths [cave path closed complete-paths units]
  (let [[^long px ^long py] (first path)]
    (if (and (not (empty? complete-paths)) (> (inc (count path)) (count (first complete-paths))))
      (list)
      (map #(conj path %)
           (filter (fn [[nx ny :as new]] (and (not (contains? closed new))
                                              (= \. (cave new))
                                              (empty? (filter (fn [[ux uy _ _]] (and (= ux nx) (= uy ny))) units))))
                   (map (fn [[^long dx ^long dy]] [(+ dx px) (+ dy py)]) deltas))))))

(defn shortest-path [cave src dests units]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY (list src)) closed #{} complete-paths (sorted-set-by 
                                                                                           (fn [[[fax fay] & ra] [[fbx fby] & rb]]
                                                                                             (let [[ax ay] (second (reverse ra))
                                                                                                   [bx by] (second (reverse rb))]
                                                                                               (compare (vector fay fax ay ax) (vector fby fbx by bx)))))]
    (if (empty? q)
      complete-paths
      (let [new-paths (new-paths cave (peek q) closed complete-paths units)]
        (recur (into (pop q) new-paths)
               (into closed (map first new-paths))
               (into complete-paths (filter #(contains? dests (first %)) new-paths)))))))

(defn next-move [cave unit squares units]
  (let [[ux uy _ _] unit]
    (second (reverse (first (shortest-path cave [ux uy] squares units))))))

(defn in-range-squares [cave i units]
  (into #{}
        (filter (fn [[x y :as sq]] (and (= \. (cave sq))
                                        (empty? (filter (fn [[ux uy _ _]] (and (= ux x) (= uy y))) units))))
                (mapcat (fn [[^long tx ^long ty _ _]] (map (fn [[^long dx ^long dy]] (vector (+ tx dx) (+ ty dy))) deltas)) (identify-targets i units)))))

(defn move [cave units i]
  (if-let [next-move (next-move cave (units i) (in-range-squares cave i units) units)]
    (let [[dx dy] next-move]
      (update units i (fn [[x y hp t]] (vector dx dy hp t))))
    units))

(defn unit-turn [n cave i units]
  (if (nil? (units i))
    units
    (let [targets (identify-targets i units)]
      (if (empty? targets)
        (throw (RuntimeException. (str "Final Score: " (final-score n units))))
        (if-let [to-attack (can-attack (units i) targets)]
          (attack to-attack units)
          (let [moved (move cave units i)]
            (if-let [to-attack (can-attack (moved i) targets)]
              (attack to-attack moved)
              moved)))))))

(defn round [[n cave units]]
  (do ;when (= 0 (mod n 50))
    (println n "elf health" (apply + (map (fn [[_ _ hp t]] (if (= t :elf) hp 0)) units)))
    (println n "goblin health" (apply + (map (fn [[_ _ hp t]] (if (= t :goblin) hp 0)) units))))
  (as-> units v
    (reading-order v)
    (into [] v)
    (vector 0 v)
    (iterate (fn [[^long i units]] [(inc i) (unit-turn n cave i units)]) v)
    (nth v (count units))
    (second v)
    (filter identity v)
    (into [] v)
    (vector (inc n) cave v)))

(defn run-game [^String map]
  (iterate round [1 (make-cave map) (make-units map)]))

(defn draw-map [map units]
  (apply str (nth
              (reduce (fn [[x y acc] c]
                        [(if (= \newline c) 0 (inc x))
                         (if (= \newline c) (inc y) y)
                         (conj acc (if-let [[_ _ _ t] (some (fn [[ux uy _ _ :as u]] (when (and (= ux x) (= uy y)) u)) units)]
                                     (if (= t :goblin) \G \E)
                                     c)
                               )])
                      [0 0 []]
                      (.replaceAll (.replaceAll map "G" ".") "E" "."))
              2)))

;; part 2

(defn attack-part2 [target units epower]
  (let [[tx ty _ _] target]
    (into []
          (map
           (fn [[ux uy ^long hp type :as unit]] (if (and (= ux tx) (= uy ty))
                                                  (if (> hp 3)
                                                    [ux uy (- hp (if (= type :goblin) epower 3)) type]
                                                    (if (= type :elf) (throw (RuntimeException. "dead elf"))))
                                                  unit))
           units))))

(defn unit-turn-part2 [n cave i units epower]
  (if (nil? (units i))
    units
    (let [targets (identify-targets i units)]
      (if (empty? targets)
        (throw (RuntimeException. (str "Final Score: " (final-score n units))))
        (if-let [to-attack (can-attack (units i) targets)]
          (attack-part2 to-attack units epower)
          (let [moved (move cave units i)]
            (if-let [to-attack (can-attack (moved i) targets)]
              (attack-part2 to-attack moved epower)
              moved)))))))

(defn round-part2 [[n cave units epower]]
  (do ;when (= 0 (mod n 50))
;;    (println n "elf health" (apply + (map (fn [[_ _ hp t]] (if (= t :elf) hp 0)) units)))
;;    (println n "goblin health" (apply + (map (fn [[_ _ hp t]] (if (= t :goblin) hp 0)) units)))
    )
  (as-> units v
    (reading-order v)
    (into [] v)
    (vector 0 v)
    (iterate (fn [[^long i units]] [(inc i) (unit-turn-part2 n cave i units epower)]) v)
    (nth v (count units))
    (second v)
    (filter identity v)
    (into [] v)
    (vector (inc n) cave v epower)))

(defn run-game-part2 [^String map epower]
  (iterate round-part2 [1 (make-cave map) (make-units map) epower]))

(try (run-game-part2 game-map 3) (catch Exception e "EX"))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  ;; part 1
  (round [[] [[1 2 3 :elf]]])
  (can-attack [1 3 5 :goblin] (identify-targets 2 [[1 2 3 :elf] [2 3 1 :elf] [3 4 5 :goblin]]))
  (attack [3 4 5 :goblin] [[1 2 3 :elf] [2 3 1 :elf] [3 4 3 :goblin]])

  (def example "######
#E..G.#
#...#.#
#.G.#G#
#######")
  
  (def example-cave  (make-cave example ))
  (def example-units (make-units example))

  (in-range-squares example-cave  0 example-units)
  (closest-square example-cave (example-units 0) (in-range-squares example-cave 0 example-units) example-units)
  (move example-cave example-units 0)

  (def example2 "#######
#.E...#
#.....#
#...G.#
#######")

  (def example-cave2  (make-cave example2 ))
  (def example-units2 (make-units example2))

  (closest-square example-cave2 (example-units2 0) (in-range-squares example-cave2 0 example-units2) example-units2)
  (next-move example-cave2  (example-units2 0)  (in-range-squares example-cave2 0 example-units2) example-units2)
  (move example-cave2 example-units2 0)

  (def example3 "#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########")

  (def example-cave3 (make-cave example3))
  (def example-units3 (make-units example3))

  (second (nth (iterate (fn [[^long i units]] [(inc i) (unit-turn example-cave3 i units)]) [0 (into [] (reading-order example-units3))]) 9))
  (nth (iterate round [0 example-cave3 example-units3]) 3)
  (count example-units3)

  (def example4 "#######   
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#   
#######")

  (run-game example4)
    (def example-cave4 (make-cave example4))
    (def example-units4 (make-units example4))

    (run-game "#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######")
(def example5 "#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########")

(run-game example5)
  (run-game "#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######")
  (def game-map "################################
#############..#################
#############..#.###############
############G..G.###############
#############....###############
##############.#...#############
################..##############
#############G.##..#..##########
#############.##.......#..######
#######.####.G##.......##.######
######..####.G.......#.##.######
#####.....#..GG....G......######
####..###.....#####.......######
####.........#######..E.G..#####
####.G..G...#########....E.#####
#####....G.G#########.#...######
###........G#########....#######
##..#.......#########....##.E.##
##.#........#########.#####...##
#............#######..#.......##
#.G...........#####........E..##
#....G........G..G.............#
#..................E#...E...E..#
#....#...##...G...E..........###
#..###...####..........G###E.###
#.###########..E.......#########
#.###############.......########
#################.......########
##################....#..#######
##################..####.#######
#################..#####.#######
################################")
  
  (run-game game-map)
  (def game-cave (make-cave game-map))
  (def game-units (make-units game-map))

  (println (draw-map game-map (nth round79 2)))
  (shortest-path game-cave [12 3] (in-range-squares game-cave 0 game-units) game-units)
  (def round79 (nth  (iterate round [1 (make-cave game-map) (make-units game-map)]) 79))

  (move (second round13) (nth round13 2) 0)
  (prn round13)
  (draw-map)
  (shortest-path (second round13) [17 7]   (in-range-squares (second round13) 0 (nth round13 2)) (nth round13 2))


  (def blocked
    "G...G
...G.
..G..
.G...
G...E")

  (def blocked-cave (make-cave blocked))
  (def blocked-units (make-units blocked))

  (shortest-path blocked-cave [0 0] (in-range-squares blocked-cave 0 blocked-units) blocked-units)
  (nth (iterate (fn [[i u]] [(inc i) (move game-cave u i)]) [0 game-units]) 1)

  
  ;; part 2
  (run-game-part2 game-map 16)

  )




