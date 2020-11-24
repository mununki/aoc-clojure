(ns first-clojure.aoc201810
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]
            [quil.core :as q]))

(def input
  (u/read-input "src/first_clojure/input/aoc201810"))

(def input-test
  (u/read-input "src/first_clojure/input/aoc201810test"))

(defn parse
  [raw]
  (map (fn [line]
         (let [trimmed (str/trim line)
               matches (re-seq #"-?\d+" trimmed)
               [x y vx vy] (map #(Integer/parseInt %) matches)]
           [x y vx vy])) raw))
(comment
  (parse input-test))

(defn adjacent?
  [a b]
  {:pre [(= (count a) 2) (= (count b) 2)]}
  (let [neighbors {:NW [-1 -1] :N [0 -1] :NE [1 -1] :W [-1 0] :E [1 0] :SW [-1 1] :S [0 1] :SE [1 1]}]
    (> (->> neighbors
            (filter (fn [[_ v]]
                      (= (compare a (vec (map + b v))) 0)))
            count) 0)))

(defn flip
  [sec points]
  (let [new-points (map (fn [point]
                          (let [[x y vx vy] point
                                new-x (+ x vx)
                                new-y (+ y vy)]
                            [new-x new-y vx vy])) points)
        msg? (every? true? (map (fn [[n-x n-y _ _]]
                                  (some true? (map (fn [[nn-x nn-y _ _]]
                                                     (adjacent? [n-x n-y] [nn-x nn-y])) new-points))) new-points))]
    (if msg?
      [sec new-points]
      (recur (inc sec) new-points))))

(defn flip-offset
  [sec points]
  (let [new-points (map (fn [point]
                          (let [[x y vx vy] point
                                new-x (+ x (* vx sec))
                                new-y (+ y (* vy sec))]
                            [new-x new-y vx vy])) points)]
    new-points))

(defn max-x-y
  [points]
  (reduce (fn [[acc-x acc-y] [x y _ _]]
            [(max acc-x x) (max acc-y y)]) [0 0] points))

(defn sort-coords
  [points]
  (sort (fn [[a-x a-y] [b-x b-y]]
          (compare [a-y a-x] [b-y b-x])) points))

(defn draw
  []
  (q/background 250)
  (q/stroke 0)
  (q/fill 0)
  (q/text (str "sec: " (+ (q/frame-count) 10000)) 10 20)
  (q/with-translation [0 0]
    (let [sec (+ (q/frame-count) 10000)]
      (if (<= sec 10605)
        (doseq [[x y _ _] (map (fn [[x y vx vy]]
                                 [x y]) (flip-offset (+ (q/frame-count) 10000) (-> input
                                                                                   parse)))]
          (q/ellipse x y 1 1))
        (q/text "Thank you!")))))

(defn run
  []
  (q/defsketch msg
    :title "Message"
    :draw draw
    :size [500 500]))

(comment
  (flip 1 (-> input parse)) ;; => [10605 [ ... ]]
  (map (fn [[x y vx vy]]
         [x y]) (second (flip 1 (-> input
                                    parse))))
  (every? true? [false true])
  (some true? [false false true false])
  (nnext [1 2 3 4])
  (adjacent? [0 0] [1 1])
  (map + [1 2] '(0 -1))
  (compare [2 2] [2 1]))
