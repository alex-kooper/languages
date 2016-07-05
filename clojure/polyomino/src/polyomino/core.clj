(ns polyomino.core
  (:require [polyomino.polyomino :as polyomino 
    :refer [add-point belongs-to normalize]])
  (:require [polyomino.point :as point])
  (:require [clojure.string :refer [lower-case]])
  (:gen-class))

(defn generate-by-adding-one-point [polyomino]
  (let [adjacent-point-deltas [[-1 0] [0 -1] [1 0] [0 1]]]
    (set
      (for [p (:points polyomino)
            [dx dy] adjacent-point-deltas
            :let [new-point (point/move p dx dy)]
            :when (not (belongs-to new-point polyomino))]
        (normalize (add-point polyomino new-point))))))

(defn generate-polyominos [n]
  (nth 
    (iterate 
      (comp set (partial mapcat generate-by-adding-one-point))
      [(polyomino/from-pairs [0 0])])
    (dec n)))

(defn format-duration [d]
  (let [duration-in-seconds (/ (double d) 1000000000)
        hours               (int (quot duration-in-seconds 3600))
        hours-r             (rem duration-in-seconds 3600)
        minutes             (int (quot hours-r 60))
        seconds             (rem hours-r 60)]  
    (format "%d:%02d:%f" hours minutes seconds)))

(defn -main [& args]
  (print "Enter number of cells: ")(flush)

  (let [n (read-string (read-line))
        start-time (System/nanoTime)
        polyominos (generate-polyominos n)
        end-time (System/nanoTime)
        duration (- end-time start-time)]
    (do
      (println (format "There are %d polyominos with %d cells." 
                       (count polyominos)
                       n))          

      (println (format "It took %s seconds to generate them." 
                       (format-duration duration)))

      (print "Would you like to see all of them [y/n]?")(flush)

      (let [answer (first (lower-case (read-line)))]
        (if (= answer \y)
          (doseq [p polyominos]
            (print p))))))
  (println))

