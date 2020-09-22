(ns tonysort.core)

(defn sort-by-start-end [startkw endkw coll]
  (->> coll
       (sort-by endkw)
       (sort-by startkw)))

(defn compare-start-to-end [startkw endkw e1 e2]
  (if (< (startkw e1) (endkw e2))
    nil
    e2))

(defn stack-segments [startkw endkw coll]
  (loop [c coll
         last-used []
         result []]
    (if-let [seg (first c)]
      (let [to-replace (first (remove nil? (map #(compare-start-to-end startkw endkw seg %) last-used)))
            prev (last last-used)
            adjust 
            (cond
              (and (nil? to-replace) (nil? prev)) 0
              (nil? to-replace) (inc (:adjust prev))
              :else (:adjust to-replace))
            adjusted-seg (assoc seg :adjust adjust)]
        (recur (rest c) (assoc last-used adjust adjusted-seg) (conj result adjusted-seg)))
      result)))

(defn sort-and-stack [startkw endkw coll]
  "Sorts linesegments given by a start-keyword and end-keyword, and if overlapping will be
stacked on top of each other, given by the keyword :adjust"
  (->> coll
       (sort-by-start-end startkw endkw)
       (stack-segments startkw endkw)))

;; (sort-and-stack :a :b [{:a 2 :b 4} {:a 1 :b 3} {:a 3 :b 6} {:a 2 :b 7} {:a 4 :b 8}])
