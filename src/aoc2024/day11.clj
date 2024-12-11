(ns aoc2024.day11
  (:require
   [clojure.core.memoize :as memo]
   [clojure.string :refer [split]]))

(declare stone-to-count-memo)

(def example-input "125 17")
(def raw-input (slurp "inputs/day11"))

(defn parse-stones [text]
  (as-> text t
    (split t #" ")
    (map parse-long t)))

(defn count-digits
  ([n] (count-digits n 1))
  ([n c]
   (if (< n 10)
     c
     (recur (/ n 10) (inc c)))))

(defn even-digits?
  [n]
  (-> n
      count-digits
      (mod 2)
      (= 0)))

(defn split-stone
  [stone]
  (let [digits (count-digits stone)
        stone-str (format "%d" stone)
        left (subs stone-str 0 (/ digits 2))
        right (subs stone-str (/ digits 2))]
    [(parse-long left) (parse-long right)]))

(defn step-stone
  [stone]
  (cond
    (= 0 stone) 1
    (even-digits? stone) (split-stone stone)
    :else (* stone 2024)))

(def step-stone-memo (memo/memo step-stone))

(defn stone-to-count ^{:clojure.core.memoize/args-fn butlast}
  [stone steps c]
  (if (<= steps 0)
    c
    (let [next (step-stone-memo stone)]
      (if (number? next)
        (stone-to-count-memo next (dec steps) c)
        (->> next
             (map #(stone-to-count-memo % (dec steps) 1))
             (reduce +))))))

(def stone-to-count-memo (memo/memo #'stone-to-count))

(defn part1
  [text]
  (->> text
    parse-stones
    (pmap #(stone-to-count-memo % 25 1))
    (reduce +)))

(defn part2
  [text]
  (->> text
    parse-stones
    (pmap #(stone-to-count-memo % 75 1))
    (reduce +)))

(let [text raw-input]
  (println "Part 1:" (part1 text))
  (println "Part 2:" (part2 text)))
