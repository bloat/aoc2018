(ns net.slothrop.aoc2018.day16
  (:require
   [clojure.java.io :as io]))

(def input (.split (slurp (io/resource "day16.txt")) "\n\n"))
(def part1-input (butlast (butlast input)))
(def part2-input (last input))

(defn addr [machine a b c]
  (assoc machine c (+ (machine a) (machine b))))

(defn addi [machine a b c]
  (assoc machine c (+ (machine a) b)))

(defn mulr [machine a b c]
  (assoc machine c (* (machine a) (machine b))))

(defn muli [machine a b c]
  (assoc machine c (* (machine a) b)))

(defn banr [machine a b c]
  (assoc machine c (bit-and (machine a) (machine b))))

(defn bani [machine a b c]
  (assoc machine c (bit-and (machine a) b)))

(defn borr [machine a b c]
  (assoc machine c (bit-or (machine a) (machine b))))

(defn bori [machine a b c]
  (assoc machine c (bit-or (machine a) b)))

(defn setr [machine a b c]
  (assoc machine c (machine a)))

(defn seti [machine a b c]
  (assoc machine c a))

(defn gtir [machine a b c]
  (assoc machine c (if (> a (machine b)) 1 0)))

(defn gtri [machine a b c]
  (assoc machine c (if (> (machine a) b) 1 0)))

(defn gtrr [machine a b c]
  (assoc machine c (if (> (machine a) (machine b)) 1 0)))

(defn eqir [machine a b c]
  (assoc machine c (if (= a (machine b)) 1 0)))

(defn eqri [machine a b c]
  (assoc machine c (if (= (machine a) b) 1 0)))

(defn eqrr [machine a b c]
  (assoc machine c (if (= (machine a) (machine b)) 1 0)))

(def ops [addr addi mulr muli banr bani borr bori setr seti gtir gtri gtrr eqir eqri eqrr])

(defn test-op [op before args after]
  (= after (apply op before (rest args))))

(defn test-ops [i]
  (let [[before args after] (.split i "\n")
        before-machine (read-string (.substring before 7))
        args (read-string (str "[" args "]"))
        after-machine (read-string (.substring after 6))]
    (count (filter #(test-op % before-machine args after-machine) ops))))

;; part2

(defn test-ops-part2 [i all-ops ops]
  (let [[before args after] (.split i "\n")
        before-machine (read-string (.substring before 7))
        args (read-string (str "[" args "]"))
        after-machine (read-string (.substring after 6))]
    (when-not (contains? all-ops (first args))
      [(first args) (filter #(test-op % before-machine args after-machine) ops)])))

(defn deduce [i ops]
  (loop [all-ops {} unassigned (into #{} ops)]
    (if (empty? unassigned)
      all-ops
      (let [try-ops (filter identity (map #(test-ops-part2 % all-ops unassigned) i))
            only-one (filter (fn [[oc ops]] (= (count ops) 1)) try-ops)
            [opcode [opfn]] (first only-one)]
        (if (or (nil? opcode) (nil? opfn))
          [unassigned all-ops]
          (recur (assoc all-ops opcode opfn) (disj unassigned opfn)))))))

(defn run [i opcodes]
  (reduce
   (fn [machine [op arg1 arg2 arg3]]
     ((opcodes op) machine arg1 arg2 arg3))
   [0 0 0 0]
   (map #(read-string (str "[" % "]")) (.split i "\n"))))

(comment
  (set! *unchecked-math* :warn-on-boxed)
  (set! *warn-on-reflection* true)

  ;; part 1
  (count (filter #(> % 2) (map test-ops part1-input)))

  ;; part 2
  (run part2-input
    (deduce part1-input ops))

  )
