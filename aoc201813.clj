(ns first-clojure.aoc201813
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]))

(def input
  (u/read-input "src/first_clojure/input/aoc201813"))

(def input-test
  (u/read-input "src/first_clojure/input/aoc201813test"))

(def char-track
  ;; track => / - \ | +
  #{\/ \- \\ \| \+})

(defn parse-track
  [raw char-t]
  (->> raw
       (map-indexed (fn [idx-y lane]
                      (map-indexed (fn [idx-x track]
                                     (cond
                                       (contains? char-t track) [[idx-x idx-y] track]
                                       (= \> track) [[idx-x idx-y] {:pos [idx-x idx-y] :dir "R" :next-inter "L"}]
                                       (= \v track) [[idx-x idx-y] {:pos [idx-x idx-y] :dir "D" :next-inter "L"}]
                                       (= \< track) [[idx-x idx-y] {:pos [idx-x idx-y] :dir "L" :next-inter "L"}]
                                       (= \^ track) [[idx-x idx-y] {:pos [idx-x idx-y] :dir "U" :next-inter "L"}]
                                       :else nil)) lane)))
       (map (fn [lane]
              (into {} lane)))
       (apply merge)))

(defn get-carts
  [tracks]
  (reduce (fn [acc [_ v]]
            (if (contains? char-track v)
              acc
              (conj acc v))) [] tracks))

(comment (get-carts (parse-track input char-track)))

(defn remove-carts
  [tracks]
  (into {} (map (fn [[coord v]]
                  (cond
                    (or (= v \\) (= v \/) (= v \-) (= v \|) (= v \+)) [coord v]
                    (= (:dir v) "R") [coord \-]
                    (= (:dir v) "D") [coord \|]
                    (= (:dir v) "L") [coord \-]
                    (= (:dir v) "U") [coord \|])) tracks)))

(comment (remove-carts (parse-track input char-track)))

(defn next-cart
  [tracks cart]
  (let [{:keys [pos dir next-inter]} cart
        [x y] pos
        cur (tracks pos)]
    (cond
      (= cur \|) (cond
                   (= dir "U") {:pos [x (- y 1)] :dir dir :next-inter next-inter}
                   (= dir "D") {:pos [x (+ y 1)] :dir dir :next-inter next-inter})
      (= cur \-) (cond
                   (= dir "R") {:pos [(+ x 1) y] :dir dir :next-inter next-inter}
                   (= dir "L") {:pos [(- x 1) y] :dir dir :next-inter next-inter})
      (= cur \/) (cond
                   (= dir "U") {:pos [(+ x 1) y] :dir "R" :next-inter next-inter}
                   (= dir "R") {:pos [x (- y 1)] :dir "U" :next-inter next-inter}
                   (= dir "D") {:pos [(- x 1) y] :dir "L" :next-inter next-inter}
                   (= dir "L") {:pos [x (+ y 1)] :dir "D" :next-inter next-inter})
      (= cur \\) (cond
                   (= dir "U") {:pos [(- x 1) y] :dir "L" :next-inter next-inter}
                   (= dir "R") {:pos [x (+ y 1)] :dir "D" :next-inter next-inter}
                   (= dir "D") {:pos [(+ x 1) y] :dir "R" :next-inter next-inter}
                   (= dir "L") {:pos [x (- y 1)] :dir "U" :next-inter next-inter})
      (= cur \+) (cond
                   (= next-inter "L") (cond
                                        (= dir "U") {:pos [(- x 1) y] :dir "L" :next-inter "S"}
                                        (= dir "R") {:pos [x (- y 1)] :dir "U" :next-inter "S"}
                                        (= dir "D") {:pos [(+ x 1) y] :dir "R" :next-inter "S"}
                                        (= dir "L") {:pos [x (+ y 1)] :dir "D" :next-inter "S"})
                   (= next-inter "S") (cond
                                        (= dir "U") {:pos [x (- y 1)] :dir "U" :next-inter "R"}
                                        (= dir "R") {:pos [(+ x 1) y] :dir "R" :next-inter "R"}
                                        (= dir "D") {:pos [x (+ y 1)] :dir "D" :next-inter "R"}
                                        (= dir "L") {:pos [(- x 1) y] :dir "L" :next-inter "R"})
                   (= next-inter "R") (cond
                                        (= dir "U") {:pos [(+ x 1) y] :dir "R" :next-inter "L"}
                                        (= dir "R") {:pos [x (+ y 1)] :dir "D" :next-inter "L"}
                                        (= dir "D") {:pos [(- x 1) y] :dir "L" :next-inter "L"}
                                        (= dir "L") {:pos [x (- y 1)] :dir "U" :next-inter "L"})))))

(defn check-collide
  [carts cart]
  (> (count (filter #(= (:pos cart) (:pos %)) carts)) 0))

(defn first-cart
  ;; 위에서부터 && left to right
  [carts]
  (reduce (fn [acc cart]
            (if (< (compare (:pos acc) (:pos cart)) 0)
              acc
              cart)) {:pos [99999 99999]} carts))

(defn rest-carts
  [carts cart]
  (filter #(not= (:pos cart) (:pos %)) carts))

(defn traverse
  [tracks]
  (fn [carts]
    (loop [l-carts carts
           moved-carts []]
      (let [this-cart (first-cart l-carts)
            moving-cart (next-cart tracks this-cart)
            other-carts (rest-carts l-carts this-cart)
            collide? (check-collide (apply conj other-carts moved-carts) moving-cart)]
        (cond
          collide? {:collision (:pos moving-cart)}
          (empty? l-carts) moved-carts
          :else (recur other-carts (conj moved-carts moving-cart)))))))

(defn find-collision
  [parsed]
  (let [tracks (remove-carts parsed)
        carts (get-carts parsed)]
    (loop [n 1]
      (let [result (nth (iterate (traverse tracks) carts) n)
            collide? (not (nil? (:collision result)))]
        (if collide?
          (:collision result)
          (recur (inc n)))))))

(defn traverse2
  [tracks]
  (fn [carts]
    (loop [l-carts carts
           moved-carts []]
      (let [this-cart (first-cart l-carts)
            moving-cart (next-cart tracks this-cart)
            other-carts (rest-carts l-carts this-cart)
            collide? (check-collide (apply conj other-carts moved-carts) moving-cart)
            next-other-carts (if collide?
                               (vec (filter #(not= (:pos %) (:pos moving-cart)) other-carts))
                               other-carts)
            next-moved-carts (if collide?
                               (vec (filter #(not= (:pos %) (:pos moving-cart)) moved-carts))
                               moved-carts)]
        (cond
          (<= (+ (count next-other-carts) (count next-moved-carts)) 1) {:survivor [next-other-carts next-moved-carts]}
          (empty? l-carts) next-moved-carts
          :else (recur next-other-carts (if collide?
                                          next-moved-carts
                                          (conj next-moved-carts moving-cart))))))))

(defn find-survivor
  [parsed]
  (let [tracks (remove-carts parsed)
        carts (get-carts parsed)]
    (loop [n 22875]
      (let [result (nth (iterate (traverse2 tracks) carts) n)
            survivor? (not (nil? (:survivor result)))
            _ (prn n)]
        (if survivor?
          (:survivor result)
          (recur (inc n)))))))

(defn part1
  []
  (find-collision (parse-track input char-track)))

(defn part2
  []
  ;; 마지막에 남은 cart가 마지막 tick을 수행했는 지 여부를 체크해서 답을 구해야 함
  (find-survivor (parse-track input char-track)))

(comment
  (part2) ;; => [116 125]
  (find-survivor (parse-track input char-track))
  (nth (iterate
        (traverse2 (remove-carts (parse-track input char-track)))
        (get-carts (parse-track input char-track))) 22879)
  ((remove-carts (parse-track input char-track)) [115 125])
  (part1))
