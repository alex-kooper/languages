(ns polyomino.polyomino
  (:require [polyomino.point :as point :refer [origin point]])
  (:require [clojure.string :as string])
  (:require [clojure.set :refer [union]]))

(defrecord Polyomino [points]
  java.lang.Comparable
    (compareTo [this other] 
      (compare (into [] (:points this)) 
               (into [] (:points other)))))

(defn polyomino [points]
  (->Polyomino (apply sorted-set points)))

(defn from-pairs [& pairs]
  (polyomino (map #(apply point %) pairs)))

(defn belongs-to [point {points :points}]
  (boolean (points point)))

(defn add-point [{points :points} point]
  (polyomino (conj points point)))
 
(defn upper-left-corner [{points :points}]
  (point
    (apply min (map :x points))
    (apply min (map :y points))))

(defn lower-right-corner [{points :points}]
  (point
    (apply max (map :x points))
    (apply max (map :y points))))

(defn width [polyomino]
  (inc (-
         (:x (lower-right-corner polyomino))
         (:x (upper-left-corner polyomino)))))

(defn height [polyomino]
  (inc (-
         (:y (lower-right-corner polyomino))
         (:y (upper-left-corner polyomino)))))

(defn move [{points :points} dx dy]
  (polyomino (map #(point/move % dx dy) points)))

(defn rotate-right 
  ([p] (rotate-right p origin))
  ([{points :points} point]
    (polyomino (map #(point/rotate-right % point) points))))

(defn rotate-left 
  ([p] (rotate-left p origin))
  ([{points :points} point]
    (polyomino (map #(point/rotate-left % point) points))))

(defn reflect-vertically 
  ([p] (reflect-vertically p 0))
  ([{points :points} x]
    (polyomino (map #(point/reflect-vertically % x) points))))

(defn reflect-horizontally 
  ([p] (reflect-horizontally p 0))
  ([{points :points} y]
    (polyomino (map #(point/reflect-horizontally % y) points))))

(defn move-to-origin [polyomino]
  (let [{:keys [x y]} (upper-left-corner polyomino)]
    (move polyomino (- x) (- y))))

(defn all-rotations [polyomino]
  (take 4
    (iterate (comp move-to-origin rotate-right) 
             (move-to-origin polyomino))))

(defn all-rigid-transformations [polyomino]
  (union
    (set (all-rotations polyomino)) 
    (set (all-rotations (reflect-vertically polyomino)))))

(defn maximum [& coll]
  (reduce #(if (pos? (compare %1 %2)) %1 %2) coll))

(defn normalize [polyomino]
  (apply maximum 
    (filter 
      #(>= (width %) (height %))
      (all-rigid-transformations polyomino))))

(defn to-string [{points :points}]
  (str
    "(from-pairs "
    (string/join
      " "
      (map #(vector (:x %) (:y %)) points))
    ")"))

(defn render [{points :points :as polyomino}]
  (let [p (move-to-origin polyomino)]
    (str 
     "\n"
     (string/join
       "\n"
       (for [r (range (height p))]
         (string/join 
           (for [c (range (width p))]
             (if ((:points p) (point c r)) "[]" "  ")))))
     "\n")))

(defmethod print-method Polyomino [polyomino ^java.io.Writer w]
  (if *print-readably*
    (.write w (to-string polyomino))
    (.write w (render polyomino))))

(def tetramino (from-pairs [0 0] [1 0] [2 0] [2 1]))

