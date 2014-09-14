(ns zone.lambda.game.board
  (:require
   [clojure.math.numeric-tower :as math]))



(def BLANK :.)

(defn c2dto1d [column-nb v]
  (let [[x y] v]
    (+ x (* column-nb y))))

(defn c1dto2d [column-nb i]
  (vector (mod i column-nb) (int (/ i column-nb))))

;;(c1dto2d 41)
;;(c2dto1d [6 0])
;;(c2dto1d [0 1])


;;(count (map #(c2dto1d %) (map #(c1dto2d %) (range (* raw-nb column-nb)))))
;;(count (map #(c1dto2d %) (range (* raw-nb column-nb))))

(defn- is-vertical? [[x1 y1] [x2 y2]]
  (zero? (- x1 x2)))



(defn- pos-between-vertical [[x1 y1] [x2 y2]]
  (let [[b1 b2] (if (= (.compareTo y2 y1) 1) [y1 y2] [y2 y1])]
      (for [a (range (inc b1) b2)] [x1 a])))

(defn pos-between-xy [[x1 y1] [x2 y2]]
  {:pre [(let [absslop (math/abs (/ (- y2 y1) (- x2 x1)))]
           ;(println absslop)
           (or (= absslop 1)
               (= absslop 0)))]}
  (let [forward? (> (- x2 x1) 0)
          slop (/ (- y2 y1) (- x2 x1))
          [step a b] (if forward? [1 x1 y1] [-1 x2 y2])
         f (fn [x] [(+ a (* 1 x)) (+ b (* slop x))])]
      (map f (range 1 (math/abs(- x2 x1))))))

(defn pos-between [p1 p2]
  (if (is-vertical? p1 p2)
    (pos-between-vertical p1 p2)
    (pos-between-xy p1 p2)))

(defn pos-between-1d [column-nb p1 p2]
  (map #(c2dto1d column-nb %) (pos-between p1 p2)))

(comment
  (pos-between [0 0] [7 7])
  (pos-between-1d [0 0] [7 7])
  (pos-between [0 0] [0 7])
  (pos-between [2 2] [7 7])
  (pos-between [0 0] [7 0])
  (pos-between [7 7] [0 0])
  (pos-between [0 7] [0 0])
  (pos-between [7 0] [0 0])
  (pos-between [7 7] [1 1])
)



(defn- char2state [raw-nb column-nb pieces-list]
  (into {}
        (filter
         #(not= BLANK (second %))
         (map
          #(vector (c1dto2d column-nb %1) %2 )
          (range (* column-nb raw-nb))
          pieces-list))))

(defn generate-line [n]
  (apply str "+" (repeat n "---+")))

(generate-line 7)

(defn render-board [raw-nb column-nb board-state]
  (let [line (generate-line column-nb)
        pieces-pos board-state ;(into {} board-state)
        ]
    (apply str "\n" line "\n"
           (map #(let [pos (c1dto2d column-nb (dec %))
                       c (name (get pieces-pos pos " "))]
                   (if (zero? (mod % column-nb))
                           (format "| %s |\n%s\n" c line)
                           (format "| %s " c))) (range 1 (inc (* column-nb raw-nb)))))))


(defn display-board [raw-nb column-nb board]
  (println (render-board raw-nb column-nb (char2state raw-nb column-nb board))))
