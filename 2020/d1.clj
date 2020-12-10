(ns first-clojure.2020.d1
  (:require [clojure.string :as str]
            [first-clojure.utils :as u]))

(def input
  (u/read-input "src/first_clojure/input/2020/d1"))

(def input-test
  (u/read-input "src/first_clojure/input/2020/d1test"))

(defn part1_2
  ([_ raw]
   (-> (for [x (map #(Integer/parseInt %) raw)
             y (map #(Integer/parseInt %) raw)
             :when (= (+ x y) 2020)]
         (* x y))
       distinct))
  ([_ _ raw]
   (-> (for [x (map #(Integer/parseInt %) raw)
             y (map #(Integer/parseInt %) raw)
             z (map #(Integer/parseInt %) raw)
             :when (= (+ x y z) 2020)]
         (* x y z))
       distinct)))

(comment
  (part1_2 input input input))
