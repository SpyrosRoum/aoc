(ns aoc2024.day10
  (:require
   [clojure.string :refer [split split-lines]]))

(def example-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(def raw-input (slurp "inputs/day10"))

(def directions [:up :right :down :left])

(defn parse-map
  [text]
  (let [lines (split-lines text)
        width (count (first lines))
        height (count lines)]
    {:map (->> lines
               (map #(split % #""))
               (map #(map parse-long %))
               (map #(into [] %))
               (into []))
     :width width
     :height height}))

(defn get-pos
  [ctx x y]
  (-> (:map ctx)
      (nth y [])
      (nth x nil)))

(defn find-potential-starts
  ([ctx] (let [zeroes (find-potential-starts ctx [] 0 0)]
           (assoc ctx :zeroes zeroes)))
  ([ctx zeroes x y]
   (cond
     (>= x (:width ctx)) (recur ctx zeroes 0 (inc y))
     (>= y (:height ctx)) zeroes
     :else
     (let [curr (get-pos ctx x y)]
       (if (= curr 0)
         (recur ctx (conj zeroes [x y]) (inc x) y)
         (recur ctx zeroes (inc x) y))))))

(defn get-next-pos
  [ctx x y dir]
  (condp = dir
    :up [x (dec y)]
    :right [(inc x) y]
    :down [x (inc y)]
    :left [(dec x) y]))

(defn find-trails
  "Unique trails can be found by passing `#{}` to `collect-into`.
  Otherwise a `[]` can be used to find all trails"
  ([collect-into ctx]
   (->> (:zeroes ctx)
        (map #(find-trails ctx % 0 collect-into))
        (zipmap (:zeroes ctx))
        (assoc ctx :trails)))
  ([ctx [x y] curr-expected trails]
   (let [curr (get-pos ctx x y)]
     (cond
       ;; We got out of bounds, so no trail
       (nil? curr) trails
       ;; We got to the end, so there is a trail
       (and (= curr-expected 9)
            (= curr 9)) (conj trails [x y])
       ;; Current is not an increment of prev, no trail
       (not= curr-expected curr) trails
       ;; Check for a trail in each direction and count them
       (= curr-expected curr) (->> directions
                                   (map #(get-next-pos ctx x y %))
                                   (map #(find-trails ctx % (inc curr-expected) trails))
                                   (apply concat trails)
                                   (into (empty trails)))))))

(defn part1
  [text]
  (->> text
       parse-map
       find-potential-starts
       (find-trails #{})
       :trails
       vals
       (map count)
       (reduce +)))

(defn part2
  [text]
  (->> text
       parse-map
       find-potential-starts
       (find-trails [])
       :trails
       vals
       (map count)
       (reduce +)))

(let [text raw-input]
  (println "Part 1:" (part1 text))
  (println "Part 2:" (part2 text)))
