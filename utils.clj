(ns first-clojure.utils)

(defn read-input [path]
  (-> path
   (slurp)
   (clojure.string/split-lines)))

(defn ccontains? [col key]
  (boolean (some #{key} col)))

(defn dups [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(comment
  (read-input "src/first_clojure/input/aoc201801")
  (ccontains? [1 2 3] 3)
  (dups [1 2 3 4 3]))
