(ns polyomino.point)

(defrecord Point [x y]
  java.lang.Comparable
    (compareTo [this other] 
      (compare [(:x this) (:y this)]
               [(:x other) (:y other)])))

(defn point [x y] (->Point x y))

(def origin (point 0 0))

(defn move [{:keys [x y]} dx dy] (point (+ x dx) (+ y dy)))

(defn rotate-left [{px :x py :y} {rx :x ry :y}] 
  (let [new-x (+ (- py ry) rx)
        new-y (+ (- rx px) ry)]
    (point new-x new-y)))

(defn rotate-right [{px :x py :y} {rx :x ry :y}] 
  (let [new-x (+ (- ry py) rx)
        new-y (+ (- px rx) ry)]
    (point new-x new-y)))

(defn reflect-vertically [{px :x py :y} x]
  (let [new-x (- (* 2 x) px)
        new-y py]
    (point new-x new-y)))

(defn reflect-horizontally [{px :x py :y} y]
  (let [new-x px
	new-y (- (* 2 y) py)]
    (point new-x new-y)))

