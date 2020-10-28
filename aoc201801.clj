(ns first-clojure.aoc201801
  (:require [first-clojure.utils :as u]))

(defn read-input []
  (with-open [rdr (clojure.java.io/reader "src/first_clojure/input/aoc201801")]
    (reduce conj [] (line-seq rdr))))

(def input-vector
  (map (fn [x] (read-string x)) (read-input)))

;; part1
(def part1
  (reduce + 0 input-vector))

(defn next-frequency [acc, num]
  (+ (if (empty? acc) 0 (last acc)) num))

(defn generate-freq [first freqs]
  (reduce (fn [acc num]
            (conj acc (next-frequency acc num))) first freqs))

;; part2
(def init-history {:last 0 :freqs #{0}})

(defn find-dups [history changes]
  (let [latest-freq (:last history)
        change (first changes)
        new-freq (+ latest-freq change)
        following-changes (rest changes)]
    (if (contains? (:freqs history) new-freq)
      new-freq
      (let [new-history (assoc history :last new-freq :freqs (conj (:freqs history) new-freq))]
        (if (empty? following-changes)
          (recur new-history input-vector)
          (recur new-history following-changes))))))

(comment
  ;; part1
  part1
  ;; part2
  (find-dups init-history input-vector)
  ;; tests
  (find-dups init-history [1 -1])
  (find-dups init-history [3 3 4 -2 -4])
  (find-dups init-history [-6 3 8 5 -6])
  (find-dups init-history [7 7 -2 -7 -4]))
