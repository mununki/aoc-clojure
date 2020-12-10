(ns first-clojure.aoc201812
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]))

(def init-state "##.####..####...#.####..##.#..##..#####.##.#..#...#.###.###....####.###...##..#...##.#.#...##.##..")

(def init-state-test "#..#.#..##......###...###")

(def input
  (u/read-input "src/first_clojure/input/2018/d12"))

(def input-test
  (u/read-input "src/first_clojure/input/2018/d12test"))

(defn find-dot-forward
  [m]
  (let [first-key (apply min (keys m))]
    (loop [f first-key]
      (if (= (m f) \#)
        f
        (recur (inc f))))))

(defn find-dot-backward
  [m]
  (let [last-key (apply max (keys m))]
    (loop [l last-key]
      (if (= (m l) \#)
        l
        (recur (dec l))))))

(defn prepend-buffer
  [m n]
  {:pre [(>= n 0)]}
  (loop [m' m
         n' n]
    (if (= n' 0)
      m'
      (let [first-key (apply min (keys m'))]
        (recur (assoc m' (- first-key 1) \.) (dec n'))))))

(defn append-buffer
  [m n]
  {:pre [(>= n 0)]}
  (loop [m' m
         n' n]
    (if (= n' 0)
      m'
      (let [last-key (apply max (keys m'))]
        (recur (assoc m' (+ last-key 1) \.) (dec n'))))))

(defn add-buffer
  [m]
  (let [first-key (apply min (keys m))
        last-key (apply max (keys m))
        loc-sharp-front (find-dot-forward m)
        loc-sharp-back (find-dot-backward m)
        num-dot-front (- loc-sharp-front first-key)
        num-dot-back (- last-key loc-sharp-back)
        needed-at-front (if (> num-dot-front 5)
                          0
                          (- 5 num-dot-front))
        needed-at-back (if (> num-dot-back 5)
                         0
                         (- 5 num-dot-back))]
    (-> m
        (prepend-buffer needed-at-front)
        (append-buffer needed-at-back))))

(defn gen-state-map
  [init]
  (add-buffer (into {} (map-indexed (fn [idx itm]
                                      [idx itm]) init))))

(comment (gen-state-map init-state-test))

(defn nilable
  [x]
  (if x
    x
    \.))

(defn scan
  [f]
  (fn [coll]
    (add-buffer (into {} (map (fn [[k v]]
                                (let [l2  (nilable (coll (- k 2)))
                                      l1  (nilable (coll (- k 1)))
                                      cur v
                                      r1  (nilable (coll (+ k 1)))
                                      r2  (nilable (coll (+ k 2)))]
                                  [k (f l2 l1 cur r1 r2)])) coll)))))

(defn parse
  [raw]
  (map (fn [line]
         (let [trimmed (str/trim line)
               [plants n] (str/split trimmed #" => ")]
           [plants n])) raw))

(defn matcher
  [notes]
  (fn [l2 l1 cur r1 r2]
    (reduce (fn [acc [note n]]
              (if (= (str l2 l1 cur r1 r2) note)
                (reduced (first (char-array n)))
                acc)) \. notes)))

(defn calc-sum
  [m]
  (reduce (fn [acc [k v]]
            (if (= v \#)
              (+ acc k)
              acc)) 0 m))

(defn part1
  []
  (calc-sum (last (take 21 (iterate (scan (matcher (parse input)))
                                    (gen-state-map init-state))))))

(defn scanner
  ;; 142 제너레이션까지는 규칙이 없다가, 그 이후 +32씩 증가
  []
  (map (fn [item]
         (apply - item)) (partition 2 1 (map calc-sum
                                             (drop 141 (take 201
                                                             (iterate (scan (matcher (parse input)))
                                                                      (gen-state-map init-state))))))))

(defn part2
  []
  (let [a (calc-sum (last (take 143
                                (iterate (scan (matcher (parse input)))
                                         (gen-state-map init-state)))))
        b (* (- 50000000000 142) 32)]
    (+ a b)))

(comment
  (part2)
  (calc-sum (last (take 143
                        (iterate (scan (matcher (parse input)))
                                 (gen-state-map init-state)))))
  (map calc-sum
       (drop 141 (take 201
                       (iterate (scan (matcher (parse input)))
                                (gen-state-map init-state)))))
  (scanner)
  (apply = (map - '(1 2 3) '(2 3 5)))
  (last (take 2 (iterate (scan (matcher (parse input-test))) (gen-state-map init-state-test))))
  (part1))
