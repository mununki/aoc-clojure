(ns first-clojure.aoc201811
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]))

(def input 4455)

(defn gen-grid
  []
  (for [x (range 0 300)
        y (range 0 300)]
    [x y]))

(defn calc-power-level
  [cell serial]
  (let [[x y] cell
        rack-id (+ x 10)
        begin (* y rack-id)
        increased (+ begin serial)
        multipled (* increased rack-id)
        hundreds (mod (quot multipled 100) 10)
        subtracked (- hundreds 5)]
    subtracked))

(defn gen-power-levels
  [grid serial]
  (into {} (map (fn [cell]
                  [cell (calc-power-level cell serial)]) grid)))

(defn calc-n-n
  [grid cell n]
  (let [[[x y] _] cell]
    (if (or (< 300 (+ x n)) (< 300 (+ y n)))
      0
      (let [sub-grid (for [rx (range x (+ x n))
                           ry (range y (+ y n))]
                       [rx ry])
            powers (map #(grid %) sub-grid)]
        (apply + powers)))))

(defn lookup
  [grid-power-level n]
  (reduce (fn [acc cell]
            (let [[_ max-power] acc
                  [[x y] _] cell
                  power (calc-n-n grid-power-level cell n)]
              (if (< max-power power)
                [[x y] power]
                acc))) [[0 0] 0] grid-power-level))

(defn part1
  [grid serial n]
  (-> (gen-power-levels grid serial)
      (lookup n)))

(defn part2
  [grid serial]
  (let [grid-power-level (gen-power-levels grid serial)
        max-powers (map (fn [n]
                          (let [_ (prn n)]
                            [n (lookup grid-power-level n)])) (range 1 301))]
    (reduce (fn [acc power]
              (let [[_ [_ max-power]] acc
                    [n-p [cell-p power-p]] power]
                (if (< max-power power-p)
                  [n-p [cell-p power-p]]
                  acc))) [1 [[0 0] 0]] max-powers)))

(comment
  (part2 (gen-grid) input) ;; => [11 [[236 268] 74]]
  (part1 (gen-grid) input 3) ;; => [[21 54] 33]
  (let [grid-power-level (gen-power-levels (gen-grid) input)]
    (map (fn [n]
           (let [_ (prn n)]
             [n (lookup grid-power-level n)])) [1 2 3 4 5 6 7 8 9 10 11 12]))
  (lookup (gen-power-levels (gen-grid) input) 10)
  (apply + [1 2 3 4])
  (calc-n-n (into {} (map (fn [cell]
                            [cell (calc-power-level cell input)]) (gen-grid))) [[21 61] 4] 3) ;; => -2
  ((into {} (map (fn [cell]
                   [cell (calc-power-level cell input)]) (gen-grid))) [21 61]) ;; => 0 -3 -5 -3 -5 4 4 3 3
  (into {} [[0 0] 1])
  (some nil? [1 2 nil])
  ({[0 0] 1 [0 1] 2} [0 2])
  (gen-grid)
  (calc-power-level [3 5] 8)
  (mod (quot 9045 100) 10))
