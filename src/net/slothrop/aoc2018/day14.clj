(ns net.slothrop.aoc2018.day14)

(def input 330121)
(def test-input [5 18 2018])

(defn new-board [board elf1 elf2]
  (let [score (+ (board elf1) (board elf2))
        digits (seq (Integer/toString score))
        recipes (map  #(- (int %) (int \0)) digits)]
    (if (second recipes)
      (conj (conj board (first recipes)) (second recipes))
      (conj board (first recipes)))))

(defn next-position [^long elf board]
  (mod (+ elf 1 (board elf)) (count board)))

(defn create-recipes [max]
  (let [real-max (+ 10 max)]
    (loop [board [3 7] elf1 0 elf2 1]
      (if (> (count board) real-max)
        (take 10 (drop max board))
        (let [board-with-new-recipes (new-board board elf1 elf2)]
          (recur
           board-with-new-recipes
           (next-position elf1 board-with-new-recipes)
           (next-position elf2 board-with-new-recipes)))))))

(defn create-recipes-part2 [search]
  (let [search-len (count search)]
    (loop [board [3 7] elf1 0 elf2 1]
      (if (and (< search-len (count board))
           (or (= search (subvec board (- (count board) search-len) (count board)))
               (= search (subvec board (- (count board) search-len 1) (dec (count board))))))
        (- (count board) search-len)
        (let [board-with-new-recipes (new-board board elf1 elf2)]
          (recur
           board-with-new-recipes
           (next-position elf1 board-with-new-recipes)
           (next-position elf2 board-with-new-recipes)))))))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  ;; part 1
  (map create-recipes test-input)
  (create-recipes input)
  
  ;; part 2
  (create-recipes-part2 [3 3 0 1 2 1])
  )
