(ns aoc2024.day09
  (:require
   [clojure.string :refer [split]]
   [clojure.tools.trace :refer [trace]]
   [data.deque :as dq]))

(def example-input "2333133121414131402")

(def raw-input (slurp "inputs/day09"))

(defn pos-comparator [a b] (<= (:pos a) (:pos b)))
(def sorted-set-by-pos (sorted-set-by pos-comparator))

(defn calc-next-pos
  [{files :files blocks :blocks}]
  (let [files-size (reduce #(+ %1 (:size %2)) 0 files)
        blocks-size (reduce #(+ %1 (:size %2)) 0 blocks)]
    (+ files-size blocks-size)))

(defn parse-file
  [state size id]
  (if (= size "0")
    state
    (let [actual-size (parse-long size)
          file {:id id :size actual-size :pos (calc-next-pos state)}]
      (update state :files #(conj % file)))))

(defn parse-block
  [state size idx]
  (if (= size "0")
    state
    (let [actual-size (parse-long size)
          block {:pos (calc-next-pos state) :size actual-size}]
      (update state :blocks #(conj % block)))))

(defn parse-disk
  [text]
  (loop [[curr & others] (split text #"")
         state {:files (sorted-set-by pos-comparator) :blocks []}
         i 0]
    (cond
      (nil? curr) state
      (even? i) (recur others (parse-file state curr (count (:files state))) (inc i))
      (odd? i) (recur others (parse-block state curr (count (:files state))) (inc i)))))

(defn defragment-file
  "Defragment the last file from `state`. Stops when file is completely absorbed or there are no blocks left."
  ([state] (let [file (last (:files state))
                 files (butlast (:files state))
                 [block & blocks] (:blocks state)
                 new-state {:files files :blocks blocks}]
             (defragment-file new-state file block)))
  ([state file block]
   (cond
     (nil? block) state
     ;; File is consumed and block is empty
     (and (<= (:size file) 0)
          (<= (:size block) 0)) state
     ;; File is consumed but there is leftover block
     (<= (:size file) 0) (update state :blocks #(into sorted-set-by-pos (conj % block)))
     ;; File is not consumed but we are out of blocks
     (<= (:size block) 0) (update state :files #(into sorted-set-by-pos (conj % file)))
     ;; More file and blocks to go
     :else
     (let [size-to-move (min (:size file) (:size block))
           new-file {:id (:id file) :size size-to-move :pos (:pos block)}
           remaining-file (update file :size #(- % size-to-move))
           remaining-block (-> block
                               (update :size #(- % size-to-move))
                               (update :pos #(+ % size-to-move)))
           new-state (update state :files #(into sorted-set-by-pos (conj % new-file)))]
       (if (<= (:size remaining-block) 0)
         (let [[block & others] (:blocks new-state)]
           (recur (assoc new-state :blocks others) remaining-file block))
         (recur new-state remaining-file remaining-block))))))

(defn defragment-disk
  [state]
  (loop [state state]
    (if (empty? (:blocks state))
      (do
        (trace "STATE" state)
        {:files (:files state)})
      (recur (defragment-file state)))))

(defn checksum-file
  [file]
  (let [file-id (:id file)
        max-pos (+ (:pos file) (:size file))]
    (loop [pos (:pos file)
           sum 0]
      (if (>= pos max-pos)
        sum
        (recur (inc pos) (+ sum (* pos file-id)))))))

(defn checksum
  [state]
  (loop [[file & others] (:files state)
         sum 0]
    (if (nil? file)
      sum
      (recur others (+ sum (checksum-file file))))))

(defn pp-state
  [state]
  (loop [[file & files] (:files state)]
    (when file
      (doseq [i (range (:size file))]
        (print (:id file)))
      (recur files)))
  (print \newline)
  state)

(defn part1
  [text]
  (->> text
       parse-disk
       defragment-disk
       pp-state
       checksum))

(let [text example-input]
  (println "Part 1:" (part1 text)))
