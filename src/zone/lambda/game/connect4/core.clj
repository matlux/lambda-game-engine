(ns zone.lambda.game.connect4.core
  (:require
   [net.matlux.utils :refer [unfold dbg]]
   [zone.lambda.game.board :as board :refer [pos-between BLANK]]
   [zone.lambda.game.engine :as engine :refer [game-step-monad-wrap play-game-seq seq-result]]
   )
  )

(def column-nb 7)
(def raw-nb 6)

;; partial application of board coordinates
(def c2dto1d (partial board/c2dto1d column-nb))
(def c1dto2d (partial board/c1dto2d column-nb))
(def pos-between-1d (partial board/pos-between-1d column-nb))

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
  [:. :. :. :. :. :. :.
   :. :. :. :. :. :. :.
   :. :. :. :. :. :. :o
   :. :. :. :. :. :o :x
   :. :x :o :x :o :o :o
   :. :x :x :o :x :x :x])

(display-board test-board)

(defn is-valid? [board] true)

(defn get-column-1d [col]
  (pos-between-1d [col 0] [col raw-nb]))

(defn stack-size [board col]
  (->>  (take-while #(not= (get board %) BLANK) (reverse (get-column-1d col))) count))

(defn stack-top [board col]
  (- raw-nb (inc (stack-size board col))))



(defn insert-token [board col token]
  (let [y (stack-top board col)
       id (c2dto1d [col y])]
    (assoc board id token)))

;;(insert-token test-board 5 "F")

;;(count (take-while #(not= (get test-board %) BLANK) (reverse (get-column-1d 0))))
;;(stack-count test-board 2)
;;(map #(stack-size test-board %) (range column-nb))
;;(map #(stack-top test-board %) (range column-nb))

(defn apply-move [board move is-player1-turn]
  (let [token (if is-player1-turn :x :o)
        ]
    (insert-token board move token)))

;;(apply-move test-board 5 false)


;;(-> (apply-move *initial-board* 0 false) (apply-move 1 true) (apply-move 0 false))

(defn test-if-finished [board]
  false)



(defn play-game-step [{:keys [board player1 player2 state-p1 state-p2 is-player1-turn] :as state}]
  (let [[step-player player-state] (if is-player1-turn [player1 state-p1] [player2 state-p2])
        { move :move player-state :state} (step-player {:board board :is-player1-turn is-player1-turn :state player-state})
        ]
    (if (not (is-valid? board))
      [false state]
      (let [new-board (apply-move board move is-player1-turn)
            finished? (test-if-finished new-board)] ;; finished is not a boolean
        [finished? (merge {:board new-board :player1 player1 :player2 player2 :is-player1-turn (not is-player1-turn) }
                          (if is-player1-turn
                            {:state-p1 player-state :state-p2 state-p2}
                            {:state-p1 state-p1 :state-p2 player-state}))]))))

(def game-step (game-step-monad-wrap play-game-step))

;;(game-step {:board initial-board :is-player1-turn true :player1 interactive-player :player2 interactive-player })
;;(game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) })
;;(take 2 (unfold game-step {:board initial-board :is-player1-turn true :player1 (create-fn [0 1 2 3]) :player2 (create-fn [0 1 2 4]) }))



(defn -main []
 (seq-result (play-game-seq game-step  {:board initial-board :player1 interactive-player :player2 interactive-player})))

;;(play initial-board p1 p2)

;;
