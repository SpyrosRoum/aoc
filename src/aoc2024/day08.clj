(ns aoc2024.day08
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.string :refer [split split-lines]]))

(def example-input "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

(def raw-input (slurp "inputs/day08"))

(defn inc-x [state] (update state :x inc))

(defn inc-y [state] (update state :y inc))

(defn zero-x [state] (assoc state :x 0))

(def inc-line (comp zero-x inc-y))

(defn add-antenna
  "Add an antenna of frequence `freq` at the `state`'s `x` and `y`"
  [state freq]
  (let [x (:x state)
        y (:y state)]
    (update-in state [:antennas freq] #(conj (or % #{}) [x y]))))

(defn parse-map
  [text]
  (loop [state {:x 0, :y 0, :antennas {}}
         [curr & tail] text]
    (condp = curr
      nil state
      \. (recur (inc-x state) tail)
      \newline (recur (inc-line state) tail)
      (recur (inc-x (add-antenna state curr)) tail))))

(defn create-ctx
  [text]
  (let [ctx (parse-map text)]
    (as-> ctx c
      (assoc c :height (inc (:y c)))
      (assoc c :width (:x c))
      (dissoc c :x :y))))

(defn within-bounds?
  [ctx [x y]]
  (and (>= x 0)
       (>= y 0)
       (< x (:width ctx))
       (< y (:height ctx))))

(defn antinodes-for-pair
  "`steps_from` and `steps_to` is the range that the diff get's multiplied
  by before being added to x/y"
  [[[x1 y1] [x2 y2]] steps-from steps-to]
  (->> (for [step (range steps-from steps-to)]
         (let [x-diff (* (- x2 x1) step)
               y-diff (* (- y2 y1) step)]
           [[(- x1 x-diff) (- y1 y-diff)]
            [(+ x2 x-diff) (+ y2 y-diff)]]))
       (apply concat)))

(defn find-antinodes
  [[_freq coords] steps-from steps-to]
  (loop [combos (combo/combinations coords 2)
         antinodes #{}]
    (if (empty? combos)
      antinodes
      (recur (rest combos) (apply conj antinodes (antinodes-for-pair (first combos) steps-from steps-to))))))

(defn part1
  [text]
  (let [ctx (create-ctx text)]
    (->> (:antennas ctx)
         (mapcat #(find-antinodes % 1 2))
         (filter #(within-bounds? ctx %))
         (reduce #(merge %1 %2) #{})
         count)))

(defn part2
  [text]
  (let [ctx (create-ctx text)]
    (->> (:antennas ctx)
         (mapcat #(find-antinodes % 0 (:height ctx)))
         (filter #(within-bounds? ctx %))
         (reduce #(merge %1 %2) #{})
         count)))

(let [text raw-input]
  (println "Part 1:" (part1 text))
  (println "Part 2:" (part2 text)))
