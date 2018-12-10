(ns net.slothrop.aoc2018.day8
  (:require [net.slothrop.aoc2018.input :as i]
            [clojure.java.io :as io])
  (:import [javax.swing JFrame JPanel SwingUtilities]
           [java.awt Dimension Graphics]))

(def input (into [] (.split (slurp (io/resource "day10.txt")) "\n")))
(def test-input (into [] (.split (slurp (io/resource "day10-example.txt")) "\n")))

(defn parse-point [line]
  (let [matcher (re-matcher #"position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>" line)]
    (when (re-find matcher)
      (into [] (map #(Integer/parseInt %) (drop 1 (re-groups matcher)))))))

(def stars (atom [0 (map parse-point input)]))

(defn move-point [[^int px ^int py ^int vx ^int vy] second]
  [(+ px (* second vx)) (+ py (* second vy)) vx vy])

(defn paint-points [^Graphics g minx maxx miny maxy]
  (let [[seconds points] @stars]
    (println (str "Seconds " seconds))
    (doseq [[x y _ _] points]
      (when (and (< minx x maxx) (< miny y maxy))
        (.drawLine g (+ maxx x) (+ maxy y) (+ maxx x) (+ maxy y))))))

(def panel (proxy [JPanel] []
             (getPreferredSize []  (Dimension. 500 500))
             (paintComponent [^Graphics g]
               (proxy-super paintComponent g)
               (paint-points g -250 250 -250 250))))

(def frame (doto (JFrame. "Message in the stars")
             (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
             (.add panel)
             (.pack)
             (.setVisible true)))

(defn advance [duration]
  (swap! stars (fn [[seconds points]]
                 (vector (+ seconds duration)
                         (map (fn [s] (move-point s duration)) points)))))

(comment
  ;; part 1 & 2

  ;; in the right area
  (advance 10250)

  ;; watch it happen
  (dotimes [n 1000]
    (advance 1)
    (SwingUtilities/invokeLater #(.repaint (.getContentPane frame)))
    (Thread/sleep 1000))

  )
