(ns first-clojure.aoc201809
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]
            [clojure.core.rrb-vector :as fv]))

(def input
  (u/read-input "src/first_clojure/input/aoc201809"))

(def input-test
  (u/read-input "src/first_clojure/input/aoc201809test"))

(defn parse
  [raw]
  (let [trimmed (str/trim raw)
        [players _ _ _ _ _ last _] (str/split trimmed #" ")]
    [players last]))

(defn insertv
  [v pos x]
  {:pre [(< pos (count v))]}
  (fv/catvec (conj (if (= pos 0)
                     []
                     (fv/subvec v 0 pos)) x) (if (<= (count v) pos)
                                               []
                                               (fv/subvec v pos (count v)))))

(defn omitv
  [v pos]
  {:pre [(< pos (count v))]}
  (fv/catvec (if (= pos 0)
               []
               (fv/subvec v 0 pos)) (if (<= (count v) (+ pos 1))
                                      []
                                      (fv/subvec v (+ pos 1) (count v)))))

(defn next-pos
  [marbles pos offset dir]
  {:pre [(< pos (count marbles)) (or (= dir "CW") (= dir "CCW"))]}
  (cond
    (= dir "CW") (if (<= (count marbles) (+ pos offset))
                   (mod (+ pos offset) (count marbles))
                   (+ pos offset))
    (= dir "CCW") (if (< pos offset)
                    (let [rem-offset (rem (- pos offset) (count marbles))]
                      (if (= rem-offset 0)
                        pos
                        (+ (count marbles) rem-offset)))
                    (- pos offset))
    :else pos))

(defn generate-marbles
  [cur pos end scores marbles]
  (if (< end cur)
    scores
    (let [next (inc cur)
          multipled? (= (mod cur 23) 0)
          next-p (if multipled?
                   (next-pos marbles pos 7 "CCW")
                   (next-pos marbles pos 2 "CW"))
          next-scores (if multipled?
                        (assoc scores cur (+ cur (if (<= (count marbles) next-p)
                                                   (marbles (- next-p (count marbles)))
                                                   (marbles next-p))))
                        scores)
          next-m (if multipled?
                   (omitv marbles next-p)
                   (insertv marbles next-p cur))
          _ (when (= (mod cur 1000000) 0)
              (prn cur))]
      (recur next next-p end next-scores next-m))))

(defn best-score
  [num-of-player last-marble]
  (sort-by (fn [[_ v]]
             v) > (reduce (fn [acc [k v]]
                            (let [player (mod k num-of-player)]
                              (if (contains? acc player)
                                (update-in acc [player] + v)
                                (assoc-in acc [player] v)))) {} (generate-marbles 1 0 last-marble {} [0]))))

(defn part1
  []
  (first (best-score 425 70848)))

(defn part2
  []
  (first (best-score 425 7084800)))

(comment
  (prn "---")
  (time (part1))
;; => [186 413188]
  (part2)

  (insertv [0] 0 1)
  (omitv [0 2 1] 1)
  (next-pos [0 16 8] 4 2 "CW")
  (nth [0 3 2] 1)
  (as-> input input
    (map parse input)))
