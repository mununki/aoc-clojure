(ns first-clojure.utils)

(defn ccontains? [col key]
  (boolean (some #{key} col)))

(defn dups [seq]
  (for [[id freq] (frequencies seq)
        :when (> freq 1)]
    id))

(comment
  (ccontains? [1 2 3] 3)
  (dups [1 2 3 4 3])
  )
