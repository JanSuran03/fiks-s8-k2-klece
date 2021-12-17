(ns fiks-s8-k2-klece.core
  (:require [clojure.string :as str]))

(def num-random-animals 30)
(def rand-range 7)
(def samples? true)

(defn random-animals [n]
  (let [rand-vec (mapcat #(repeat 3 %) (range rand-range))]
    (loop [i 0
           ret []
           cur 0]
      (if (< i n)
        (let [cur (+ cur (rand-nth rand-vec))]
          (recur (inc i)
                 (conj ret cur)
                 cur))
        ret))))

(defn read-input []
  (let [[_ input max-ferocity-diff] (-> "input.txt" slurp (str/split #"\n"))
        split-ferocities (if samples?
                           (random-animals num-random-animals)
                           (as-> input <>
                                 (str/split <> #"\ ")
                                 (mapv read-string <>)))]
    [split-ferocities (read-string max-ferocity-diff)]))


(defn -main [& args]
  (let [[ferocities max-ferocity-diff] (read-input)
        [first-ferocity & sorted-ferocities] (sort ferocities)]
    (loop [min-ferocity first-ferocity
           current-cage [first-ferocity]
           ret []
           remaining sorted-ferocities]
      (if (seq remaining)
        (let [next-ferocity (first remaining)]
          (if (<= (- next-ferocity min-ferocity)
                  max-ferocity-diff)
            (recur min-ferocity
                   (conj current-cage next-ferocity)
                   ret
                   (next remaining))
            (recur next-ferocity
                   [next-ferocity]
                   (conj ret current-cage)
                   (next remaining))))
        (conj ret current-cage)))))
