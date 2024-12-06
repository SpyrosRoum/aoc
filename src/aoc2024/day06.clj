(ns aoc2024.day06
  (:require
   [clojure.string :refer [split split-lines]]))

(def example-input "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def raw-input (slurp "inputs/day06"))

(defn parse-map [lines]
  "Parse the input lines to a map from coords to thing. (Blank spaces not included)"
  (loop [[line & rest-lines] lines
         [x y] [0 0]
         res-map {}]
    (cond
      (every? empty? [line rest-lines])
      ;; Nothing left in current line and no others lines left
      res-map
      ;; Current line empty. Recur by resetting X and inc y
      (empty? line) (recur rest-lines [0 (inc y)] res-map)
      ;; Stil have things in line
      :else
      (let [curr (first line)]
        (condp = curr
          \. (recur (cons (rest line) rest-lines) [(inc x) y] res-map)
          \# (recur (cons (rest line) rest-lines) [(inc x) y] (update res-map :obstacles (fn [existing] (conj (or existing #{}) [x y]))))
          \^ (recur (cons (rest line) rest-lines) [(inc x) y] (assoc res-map :guard {:pos [x y] :dir :up})))))))

(defn parse-input
  "Returns a map like {:obstacles #{[x1 y1] [x2 y2] ...} :guard {:pos [x y] :direction :up} :width w :height h :obstacles-visited {[x y] #{:up :left}}}
  `:obstacles-visited` is a map from obstacle coords to a set of directions we were facing when visiting the obstacle"
  [text]
  (let [lines (split-lines text)
        height (count lines)
        width (count (split (first lines) #""))
        parsed-map (parse-map lines)]
    (merge parsed-map {:width width :height height :obstacles-visited {}})))

(defn rot90 [dir]
  (condp = dir
    :up :right
    :right :down
    :down :left
    :left :up))

(defn calc-next-pos
  "Calculate the guards next position based *only* on their current position and direction"
  [state]
  (let [[curr-x curr-y] (get-in state [:guard :pos])
        guard-dir (get-in state [:guard :dir])]
    (condp = guard-dir
      :up [curr-x (dec curr-y)]
      :down [curr-x (inc curr-y)]
      :right [(inc curr-x) curr-y]
      :left [(dec curr-x) curr-y])))

(defn is-out?
  "Check if `pos` is out of the map"
  [state pos]
  (let [[x y] pos
        width (:width state)
        height (:height state)]
    (or
      (< x 0)
      (>= x width)
      (< y 0)
      (>= y height))))

(defn is-obstacle?
  "Check if `pos` is an obstacle"
  [state pos]
  (contains? (:obstacles state) pos))

(defn visited?
  "Check if the obstacle at `pos` has been visited before while the guard had the same direction as now"
  [state pos]
  (let [guard-dir (get-in state [:guard :dir])
        previous-directions (get-in state [:obstacles-visited pos])]
    (and previous-directions
         (previous-directions guard-dir))))

(defn calc-path
  "Returns `nil` if a loop is detected, otherwise a set of positions we visited to get out"
  ([state] (calc-path state #{(get-in state [:guard :pos])}))
  ([state path]
   (let [expected-pos (calc-next-pos state)
         guard-dir (get-in state [:guard :dir])]
     (cond
       ;; We are at borders facing an exit, so we are done.
       (is-out? state expected-pos) path
       (is-obstacle? state expected-pos)
       (when-not (visited? state expected-pos)
         (recur (-> state
                    (update-in [:obstacles-visited expected-pos] #(conj (or % #{}) guard-dir))
                    (update-in [:guard :dir] rot90))
                path))
       :else (recur (assoc-in state [:guard :pos] expected-pos) (conj path expected-pos))))))

(defn part1 [text]
  (let [state (parse-input text)
        path (calc-path state)]
    (count path)))

(defn part2 [text]
  (let [state (parse-input text)
        guard-pos (get-in state [:guard :pos])
        path-with-guard (calc-path state)
        path (disj path-with-guard guard-pos)]
    (->> path
         (map (fn [pos] (update state :obstacles #(conj % pos))))
         (pmap calc-path)
         (filter nil?)
         count)))

(let [text raw-input]
  (println "Part1:" (part1 text))
  (println "Part2:" (part2 text)))
