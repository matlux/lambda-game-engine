(ns zone.lambda.game.connect4.core
  (:require
   [net.matlux.utils :refer [unfold dbg]]
   [zone.lambda.game.board :as board :refer [pos-between pos-between-incl BLANK]]
   [zone.lambda.game.engine :as engine :refer [game-step-monad-wrap play-game-seq seq-result play-scenario-seq create-fns-from-scenario]]
   )
  )

(def column-nb 7)
(def raw-nb 6)

;; partial application of board coordinates
(def c2dto1d (partial board/c2dto1d column-nb))
(def c1dto2d (partial board/c1dto2d column-nb))
(def pos-between-1d (partial board/pos-between-1d column-nb))
(def pos-xy-within-board? (partial board/pos-xy-within-board? column-nb raw-nb))

;; need both column-nb and raw-nb
(def display-board (partial board/display-board column-nb raw-nb))

(def interactive-player (engine/interactive-player display-board))

(def initial-board
  [:. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :. :. :. :.])
(def test-board
  [:. :. :. :. :. :. :o
   :. :. :. :. :. :o :x
   :. :. :. :. :o :x :o
   :. :. :. :. :x :o :x
   :. :x :o :x :o :o :o
   :. :x :x :o :x :x :x])

(def test-board2
  [:. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :o :o :. :.
   :. :. :o :x :o :x :.
   :. :o :x :x :o :x :.
   :o :x :o :x :x :x :.])
(def test-board3
  [:. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :o :. :. :.
   :. :. :o :x :. :. :.
   :. :o :x :x :. :. :.
   :o :o :x :x :. :. :.])

(defn get-column-1d [col]
  (pos-between-1d [col -1] [col raw-nb]))

;;(get-column-1d 6)
;;(pos-between [6 -1] [6 raw-nb])

(defn stack-size [board col]
  (->>  (take-while #(not= (get board %) BLANK) (reverse (get-column-1d col))) count))

(defn stack-top [board col]
  (- raw-nb (inc (stack-size board col))))

(defn is-valid? [board move]
  (and (< move column-nb) (>= move 0) (< (stack-size board move) raw-nb)))

;; (is-valid? test-board 6)
;; (stack-size test-board 6)

(defn insert-token [board col token]
  (let [y (stack-top board col)
       id (c2dto1d [col y])]
    (assoc board id token)))

;;(insert-token test-board 5 "F")

;;(count (take-while #(not= (get test-board %) BLANK) (reverse (get-column-1d 0))))
;;(stack-count test-board 2)
;;(map #(get-column-1d %) (range column-nb))
;;(map #(stack-size test-board %) (range column-nb))
;;(map #(stack-top test-board %) (range column-nb))
;;(map #(is-valid? test-board %) (range column-nb))

(defn apply-move [board move is-player1-turn]
  (let [token (if is-player1-turn :x :o)
        ]
    (insert-token board move token)))

;;(apply-move test-board 5 false)


;;(-> (apply-move *initial-board* 0 false) (apply-move 1 true) (apply-move 0 false))

(defn get-horizontals [board]
  (map #(pos-between [-1 %] [column-nb %]) (range 0 raw-nb)))

(defn get-verticals [board]
  (map #(pos-between [% -1] [% raw-nb]) (range 0 column-nb)))

(defn get-diagonals [board]
  (->> (for [x (range 0 (* column-nb 2))
         ]
     (filter #(pos-xy-within-board? %)
             (pos-between-incl [x 0] [0 x])))
       (remove empty?)))

;;(pos-between-incl [2 0] [0 2])

(defn get-diagonals2 [board]
  (->> (for [y (range 0 (* raw-nb 2))
         ]
     (filter #(pos-xy-within-board? %)
             (pos-between-incl [(- (dec column-nb) y) 0] [(dec column-nb) y])))
       (remove empty?)))

;; (get-horizontals test-board)

;; (get-verticals test-board)
;; (map count (get-diagonals test-board))
;; (map count (get-diagonals2 test-board))
;; (map count (get-diagonals test-board3))
;; (map count (get-diagonals2 test-board3))

(defn get-all-lines [board]
               (concat
                (get-horizontals board)
                (get-verticals board)
                (get-diagonals board)
                (get-diagonals2 board)
                ) )

(defn get-piece [board pos]
  (get board (c2dto1d pos)))

(defn four-in-row [line board]
  (= 4 (count (reduce (fn [acc coord]
                        (let [piece (get-piece board coord)]
                          (if (= (count acc) 4)
                           (reduced acc)
                           (if (= piece BLANK)
                             []
                             (if (and (seq acc) (= (first acc) piece))
                               (conj acc piece)
                               [piece])))))
                      []
                      line))))

(defn mark-four [line board]
  (reduce (fn [acc coord]
            (let [piece (get-piece board coord)
                  last-piece (ffirst acc)]
              (if (= (count acc) 4)
                (reduced acc)
                (if (= piece BLANK)
                  []
                  (if (and (seq acc) (= last-piece piece))
                    (conj acc [piece coord])
                    [[piece coord]])))))
          []
          line))


;; (get-piece test-board [5 3])

;; (= 4 (count (reduce (fn [acc coord]
;;                         (let [piece (get-piece test-board coord)]
;;                           (if (= (count acc) 4)
;;                            (reduced acc)
;;                            (if (= piece BLANK)
;;                              []
;;                              (if (and (seq acc) (= (first acc) piece))
;;                                (conj acc piece)
;;                                [piece])))))
;;                       []
;;                       (list [3 5] [4 4] [5 3] [6 2]))))
;;(four-in-row [:. :x :o :o :o  :x])
(four-in-row [[3 5] [4 4] [5 3] [6 2]] test-board)
(four-in-row (list [3 5] [4 4] [5 3] [6 2]) test-board)
(four-in-row (list [3 5] [4 4] [5 3] [6 3]) test-board)
(mark-four [[3 5] [4 4] [5 3] [6 2]] test-board)
(mark-four [[0 0] [1 0] [2 0] [3 0] [4 0]] test-board)

(reduce (fn [acc e]
          (if (= (count acc) 4)
            (reduced acc)
            (if (and (seq acc) (= (first acc) e))
                              (conj acc e)
                              [e])))
        []
        [:. :x :o :o :o :o :x])


(defn find-fours [board]
  (for [line (get-all-lines board)
        :when (four-in-row line board)]
    (mark-four line board)))

(defn find-ffour [board]
  (first (find-fours board)))

(find-fours test-board)
(find-ffour test-board)
(find-fours test-board2)
(find-ffour test-board2)
(find-fours test-board3)
;; (count (get-all-lines test-board3))
;; (for [line (get-all-lines test-board3)
;;         :when (four-in-row line test-board3)]
;;   line)


;; (([0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0]) ([0 1] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1]) ([0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2]) ([0 3] [1 3] [2 3] [3 3] [4 3] [5 3] [6 3]) ([0 4] [1 4] [2 4] [3 4] [4 4] [5 4] [6 4]) ([0 5] [1 5] [2 5] [3 5] [4 5] [5 5] [6 5]) ([0 0] [0 1] [0 2] [0 3] [0 4] [0 5]) ([1 0] [1 1] [1 2] [1 3] [1 4] [1 5]) ([2 0] [2 1] [2 2] [2 3] [2 4] [2 5]) ([3 0] [3 1] [3 2] [3 3] [3 4] [3 5]) ([4 0] [4 1] [4 2] [4 3] [4 4] [4 5]) ([5 0] [5 1] [5 2] [5 3] [5 4] [5 5]) ([6 0] [6 1] [6 2] [6 3] [6 4] [6 5]) ([1 1]) ([1 2] [2 1]) ([1 3] [2 2] [3 1]) ([1 4] [2 3] [3 2] [4 1]) ([1 5] [2 4] [3 3] [4 2] [5 1]) ([2 5] [3 4] [4 3] [5 2] [6 1]) ([3 5] [4 4] [5 3] [6 2]) ([4 5] [5 4] [6 3]) ([5 5] [6 4]) ([6 5]) ([5 1]) ([4 1] [5 2]) ([3 1] [4 2] [5 3]) ([2 1] [3 2] [4 3] [5 4]) ([1 1] [2 2] [3 3] [4 4] [5 5]) ([0 1] [1 2] [2 3] [3 4] [4 5]) ([0 2] [1 3] [2 4] [3 5]) ([0 3] [1 4] [2 5]) ([0 4] [1 5]) ([0 5]))

(find-ffour test-board3)
(find-fours initial-board)
(find-ffour initial-board)

;;(map #(mark-four % test-board) (get-all-lines test-board))

(defn test-if-finished [board]
  (find-ffour board))

(test-if-finished test-board)
(test-if-finished test-board2)
(test-if-finished test-board3)
(test-if-finished [:. :. :. :. :. :. :.
                   :. :. :. :. :. :. :.
                   :. :. :. :o :. :. :.
                   :. :. :o :x :. :. :.
                   :. :o :x :x :. :. :.
                   :o :o :x :x :. :. :.])

(defn forfeit [is-player1-turn]
  (if is-player1-turn [0 1] [1 0]))
(def opposite-color-wins forfeit)


(defn play-game-step [{:keys [board player1 player2 state-p1 state-p2 is-player1-turn] :as state}]
  (cond (or (nil? player1) (nil? player1)) nil
        (dbg (test-if-finished (dbg board)))
        (do
          (println "check-mate!")
          [true {:score (opposite-color-wins is-player1-turn) :board board :result :user-win}])
        :else (let [[step-player player-state] (if is-player1-turn [player1 state-p1] [player2 state-p2])
         { move :move player-state :state} (step-player {:board board :is-player1-turn is-player1-turn :state player-state})
         ]
     (if (is-valid? board move)
       (let [new-board (apply-move board move is-player1-turn)
             ] ;; finished is not a boolean
         [false (merge {:board new-board :player1 player1 :player2 player2 :is-player1-turn (not is-player1-turn) }
                           (if is-player1-turn
                             {:state-p1 player-state :state-p2 state-p2}
                             {:state-p1 state-p1 :state-p2 player-state}))])
       [true {:score (forfeit is-player1-turn) :board board :result :invalid-move}]))))

(def game-step (game-step-monad-wrap play-game-step))

;;(game-step {:board initial-board :is-player1-turn true :player1 interactive-player :player2 interactive-player })
;;(game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) })
;;(take 2 (unfold game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) }))
(defn play-scenario-seq2 [initial-board step scenario] (let [[f1 f2] (create-fns-from-scenario scenario)]
                                 (let [result (play-game-seq step {:board initial-board :player1 f1 :player2 f2})]
                                   (take (count scenario) result))))

;; (->> (play-scenario-seq2 initial-board game-step  [1 0 2 1 2 2 3 3 3 3 0])
;;      last (#(do (println %) (identity %))) second  :board display-board)

;; (test-if-finished [:. :. :. :. :. :. :.
;;                    :. :. :. :. :. :. :.
;;                    :. :. :. :o :. :. :.
;;                    :. :. :o :x :. :. :.
;;                    :. :o :x :x :. :. :.
;;                    :o :o :x :x :. :. :.])


(defn -main []
 (println (seq-result (play-game-seq game-step  {:board initial-board :player1 interactive-player :player2 interactive-player}))))

;;(play initial-board p1 p2)

;;
