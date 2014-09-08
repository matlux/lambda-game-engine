(ns connect4.core)

(def initial-board
  [[] [] [] [] [] [] []])

(defn is-valid? [board] true)

(defn apply-move [board move is-player1-turn]
  (let [token (if is-player1-turn "x" "o")]
    (assoc board move (conj (get board move) token))))

(apply-move initial-board 0 false)

;;(macroexpand '(let [[a b :as tomvec] [1 2]] true))
;;(macroexpand '(let [a [1 2]] true))
;;(macroexpand '(fn foo [[a b :as tomvec]] true))

(defn test-if-finished [board]
  false)


(defn interactive-player [{board :board is-player1-turn :is-player1-turn}]
  ;;(display-board board)
  (let [move (read-string (read-line))]
    move))

(defn game-step [{:keys [board player1 player2 is-player1-turn] :as state}]
  (let [step-player (if is-player1-turn player1 player2)
        move (step-player {:board board :is-player1-turn is-player1-turn})
        ]
    (if (is-valid? board)
      [false state]
      (let [new-board (apply-move board move)
            finished? (test-if-finished new-board)] ;; finished is not a boolean
        [finished? {:board new-board :player1 player1 :player2 player2 :is-player1-turn (not is-player1-turn) }]))))

;;(play initial-board p1 p2)

;;
