(ns connect4.core
  (:require
   [clojure.math.numeric-tower :as math]
   [clojure.algo.monads :as m :refer [domonad state-m fetch-state fetch-val]])
  )

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn unfold [g seed]
  (->> (g seed)
       (iterate (comp g second))
       (take-while identity)
       (map first)))

(def ^:dynamic *column-nb* 7)
(def ^:dynamic *raw-nb* 6)
;; (def MAX_X (dec *column-nb*))
;; (def MAX_Y (dec *raw-nb*))
(def BLANK \.)

(defn c2dto1d [v]
  (let [[x y] v]
    (+ x (* (inc *raw-nb*) y))))

(defn c1dto2d [i]
  (vector (mod i *column-nb*) (int (/ i (inc *raw-nb*)))))

;;(c1dto2d 41)
;;(c2dto1d [6 0])
;;(c2dto1d [0 1])


;;(count (map #(c2dto1d %) (map #(c1dto2d %) (range (* *raw-nb* *column-nb*)))))
;;(count (map #(c1dto2d %) (range (* *raw-nb* *column-nb*))))

(defn- is-vertical? [[x1 y1] [x2 y2]]
  (zero? (- x1 x2)))



(defn- pos-between-vertical [[x1 y1] [x2 y2]]
  (let [[b1 b2] (if (= (.compareTo y2 y1) 1) [y1 y2] [y2 y1])]
      (for [a (range (inc b1) b2)] [x1 a])))

(defn- pos-between-xy [[x1 y1] [x2 y2]]
  {:pre [(let [absslop (math/abs (/ (- y2 y1) (- x2 x1)))]
           ;(println absslop)
           (or (= absslop 1)
               (= absslop 0)))]}
  (let [forward? (> (- x2 x1) 0)
          slop (/ (- y2 y1) (- x2 x1))
          [step a b] (if forward? [1 x1 y1] [-1 x2 y2])
         f (fn [x] [(+ a (* 1 x)) (+ b (* slop x))])]
      (map f (range 1 (math/abs(- x2 x1))))))

(defn- pos-between [p1 p2]
  (if (is-vertical? p1 p2)
    (pos-between-vertical p1 p2)
    (pos-between-xy p1 p2)))

(defn pos-between-1d [p1 p2]
  (map #(c2dto1d %) (pos-between p1 p2)))

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



(defn char2state [pieces-list]
  (into {}
        (filter
         #(not= BLANK (second %))
         (map
          #(vector (c1dto2d %1) %2 )
          (range (* *column-nb* *raw-nb*))
          pieces-list))))

;;(char2state initial-board)

(defn generate-line [n]
  (apply str "+" (repeat n "---+")))

(generate-line 7)

(defn render-board [board-state]
  (let [line (generate-line *column-nb*)
        pieces-pos board-state ;(into {} board-state)
        ]
    (apply str "\n" line "\n"
           (map #(let [pos (c1dto2d (dec %))
                       c (get pieces-pos pos " ")]
                   (if (zero? (mod % *column-nb*))
                           (format "| %s |\n%s\n" c line)
                           (format "| %s " c))) (range 1 (inc (* *column-nb* *raw-nb*)))))))


(defn display-board [board]
  (println (render-board (char2state board))))

;; (defn display-board [board]
;;   (println board))

(def initial-board
  [\. \. \. \. \. \. \.
   \. \. \. \. \. \. \.
   \. \. \. \. \. \. \.
   \. \. \. \. \. \. \.
   \. \. \. \. \. \. \.
   \. \. \. \. \. \. \.])
(def test-board
  [\. \. \. \. \. \. \.
   \. \. \. \. \. \. \.
   \. \. \. \. \. \. \o
   \. \. \. \. \. \o \x
   \. \x \o \x \o \o \o
   \. \x \x \o \x \x \x])

(defn is-valid? [board] true)

(defn get-column-1d [col]
  (pos-between-1d [col 0] [col *raw-nb*]))

(defn stack-size [board col]
  (->>  (take-while #(not= (get board %) BLANK) (reverse (get-column-1d col))) count))

(defn stack-top [board col]
  (- *raw-nb* (inc (stack-size board col))))


(c2dto1d [2 5])

(let [y (stack-top test-board 5)
      id (c2dto1d [5 y])]
  (assoc test-board id "w"))

(defn insert-token [board col token]
  (let [y (stack-top board col)
       id (c2dto1d [col y])]
    (assoc board id token)))

(insert-token test-board 5 "F")

;;(count (take-while #(not= (get test-board %) BLANK) (reverse (get-column-1d 0))))
;;(stack-count test-board 2)
;;(map #(stack-size test-board %) (range *column-nb*))
;;(map #(stack-top test-board %) (range *column-nb*))

(defn apply-move [board move is-player1-turn]
  (let [token (if is-player1-turn \x \o)
        ]
    (insert-token board move token)))

;;(apply-move test-board 5 false)


;;(-> (apply-move initial-board 0 false) (apply-move 1 true) (apply-move 0 false))

(defn test-if-finished [board]
  false)



(defn interactive-player [{board :board is-player1-turn :is-player1-turn}]
  (display-board board)
  (let [move (read-string (read-line))]
    {:move move}))

;;(interactive-player {:board initial-board :is-player1-turn true :player1 initial-board :player2 initial-board})

(defn play-game-step [{:keys [board player1 player2 state-p1 state-p2 is-player1-turn] :as state}]
  (let [[step-player player-state] (if is-player1-turn [player1 state-p1] [player2 state-p2])
        { move :move player-state :state} (dbg (step-player {:board board :is-player1-turn is-player1-turn :state player-state}))
        ]
    (if (not (is-valid? board))
      [false state]
      (let [new-board (apply-move board move is-player1-turn)
            finished? (test-if-finished new-board)] ;; finished is not a boolean
        [finished? (merge {:board new-board :player1 player1 :player2 player2 :is-player1-turn (not is-player1-turn) }
                          (if is-player1-turn
                            {:state-p1 player-state :state-p2 state-p2}
                            {:state-p1 state-p1 :state-p2 player-state}))]))))


(defn game-step-monad-wrap [game-step]
  (domonad state-m
           [res (fetch-val :result)
            a game-step
            s (fetch-state)
            :when (nil? res)]
           [a s]

           ))

(def game-step (game-step-monad-wrap play-game-step))

;;(game-step {:board initial-board :is-player1-turn true :player1 interactive-player :player2 interactive-player })
;;(game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) })
;;(take 2 (unfold game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) }))



;; this function is central to the chess engine
(defn game-seq [monadic-step init-state]
  (unfold
   monadic-step
   init-state
   ))

(defn play-game-seq [step game-init]
  (let [state (merge {:board initial-board :is-player1-turn true :game-id (str (java.util.UUID/randomUUID))} game-init)]
    (game-seq step state)))


(defn- every-nth [coll n]
  (map (fn [[i e]] e) (filter (fn [[i e]] (zero? (mod i n))) (map-indexed (fn [i e] [i e]) coll))))

(defn- create-fn [moves]
  (fn [{b :board c :is-player1-turn s :state}]
      (let [move-seq (if (nil? s)
                       moves
                       s)]
        {:move (first move-seq) :state (next move-seq)})))


;;((create-fn [0 1 2 4]) {:board initial-board :is-player1-turn true})

(defn create-fns-from-scenario [moves]
  (let [white-moves (every-nth moves 2)
        black-moves (every-nth (drop 1 moves) 2)]
    [(create-fn white-moves)
     (create-fn black-moves)]))

(defn play-scenario-seq [step scenario] (let [[f1 f2] (create-fns-from-scenario scenario)]
                                 (let [result (play-game-seq step {:f1 f1 :f2 f2})]
                                   (take (count scenario) result))))

(defn seq-result [s]
  (-> s
      last
      second))

;;(take 8 (play-game-seq game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) }))

(defn -main []
 (seq-result (play-game-seq game-step  {:player1 interactive-player :player2 interactive-player})))

;;(play initial-board p1 p2)

;;
