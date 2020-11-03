(ns first-clojure.aoc201805
  (:require [first-clojure.utils :as u]
            [clojure.string :as str]))

(def input
  (u/read-input "src/first_clojure/input/aoc201805"))

(defn reactor
  [polymer]
  (apply str (map char (reduce (fn [acc unit]
                                 (cond
                                   (empty? acc) (conj acc unit)
                                   :else (if (= (Math/abs (- (last acc) unit)) 32)
                                           (vec (drop-last acc))
                                           (conj acc unit)))) [] polymer))))

(defn int-of-string
  [s]
  (map int s))

(defn part1
  []
  (-> input
      first
      int-of-string
      reactor
      count))

(defn remove-polymer
  [unit polymer]
  (remove #(or (= unit %) (= (+ unit 32) %)) polymer))

(defn seq-polymer
  [polymer]
  (map #(let [removed-polymer (remove-polymer % polymer)]
          (count (reactor removed-polymer))) (range 65 91)))

(defn shortest-polymer
  [counts]
  (reduce min counts))

(defn part2
  []
  (-> input
      first
      int-of-string
      seq-polymer
      shortest-polymer))

(comment
  (map int "dabAcCaCBAcCcaDA")
  (- (int \A) (int \a)) ;; => -32
  (reactor "dabAcCaCBAcCcaDA")
  (- (int \A) (int \Z)) ;; 65 , 97 ~ 90 , 122
  (range 65 91)
  (remove #(or (= 65 %) (= 66 %)) [65 66 67])
  (seq-polymer input))
