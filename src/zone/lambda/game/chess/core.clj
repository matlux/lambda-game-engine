(ns zone.lambda.game.chess.core
  (:use [clojail.core :only [sandbox]]
        [clojail.testers :only [blacklist-symbols blacklist-objects secure-tester]])
  (:require [net.matlux.utils :refer [unfold dbg display-assert]]
            [zone.lambda.game.board :as board :refer [pos-between pos-between-xy BLANK

                                                      is-white? is-black?  file-component]]
            [zone.lambda.game.engine :as engine :refer [game-step-monad-wrap play-game-seq seq-result]]
            [clojure.core.async :refer [<! >! go]]
            )
  (:import clojure.lang.PersistentVector))


(def column-nb 8)
(def raw-nb 8)

;; partial application of board coordinates
(def c2dto1d (partial board/c2dto1d column-nb))
(def c1dto2d (partial board/c1dto2d column-nb))
(def rank2coord (partial board/rank2coord column-nb))
(def pos2coord (partial board/pos2coord column-nb))
(def index-xy (partial board/index-xy column-nb))

;; need both column-nb and raw-nb
(def coord2pos (partial board/coord2pos column-nb raw-nb))
(def lookup-xy (partial board/lookup-xy column-nb raw-nb))
(def lookup (partial board/lookup column-nb raw-nb))
(def collid? (partial board/collid? column-nb raw-nb))
(def collid-oposite? (partial board/collid-oposite? column-nb raw-nb))
(def collid-self? (partial board/collid-self? column-nb raw-nb))
(def nothing-between (partial board/nothing-between column-nb raw-nb))
(def pos-xy-within-board? (partial board/pos-xy-within-board? column-nb raw-nb))
(def board2xy-map-piece (partial board/board2xy-map-piece column-nb raw-nb))
(def display-board (partial board/display-board column-nb raw-nb))




(defn initial-board []
  [:r :n :b :q :k :b :n :r
   :p :p :p :p :p :p :p :p
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :P :P :P :P
   :R :N :B :Q :K :B :N :R])

(defn test-board1 []
  [:r :n :b :q :k :b :n :r
   :p :p :p :p :p :p :p :p
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :N :. :. :. :. :.
   :P :P :P :P :P :P :P :P
   :R :. :B :Q :K :B :N :R])

(defn test-board2 []
  [:r :n :. :q :k :b :n :r
   :p :. :p :p :p :p :p :p
   :. :. :. :. :. :. :. :.
   :. :K :. :. :. :. :. :.
   :. :p :. :. :. :. :. :.
   :. :. :. :b :. :. :. :.
   :P :. :P :. :. :P :P :P
   :R :. :B :Q :K :B :N :R])

(defn check-mate-test []  ;; it's blacks turn, king is in check. no move will save him => check mate
  [:r :. :b :q :k :b :n :r
   :p :p :p :p :. :Q :p :p
   :. :. :n :. :. :p :. :.
   :. :. :. :. :p :. :. :.
   :. :. :B :. :P :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :N :R])

(defn in-check-test []  ;; it's blacks turn, king is in check. no move will save him => check mate
  [:r :. :b :q :. :b :n :r
   :p :p :p :p :k :Q :p :p
   :. :. :n :. :. :p :. :.
   :. :. :. :. :p :. :. :.
   :. :. :B :. :P :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :N :R])

(defn could-become-in-check-test []  ;; it's blacks turn, king is in check. no move will save him => check mate
  [:r :. :b :q :k :b :n :r
   :p :p :p :p :. :p :p :p
   :. :. :n :. :. :. :. :.
   :. :. :. :. :p :. :. :Q
   :. :. :B :. :P :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :N :R])

(defn en-passant-check-test [] ;; it's white's turn
  [:r :. :b :q :k :b :n :r
   :p :p :. :. :. :p :p :p
   :. :. :. :. :. :. :. :.
   :n :. :p :P :p :. :. :Q
   :. :. :B :. :. :. :. :.
   :. :. :. :. :. :. :. :N
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :. :R])





(def white-turn true)
(def black-turn false)
(def black black-turn)
(def white white-turn)
(def last-move-was-invalid true)
(def last-move-was-valid false)
(def check-mate true)


(defn movemap2move [[from {to :move-to}]]
  [from to])

(defn move-xy2move [move-xy] (let [[from to] move-xy] [(coord2pos from) (coord2pos to)]))
(defn move2move-xy [move] (if (nil? move) nil (let [[from to] move] [(pos2coord from) (pos2coord to)])))
(defn move-xy2move-vec [coll] (map (fn [[from to]] [(coord2pos from) (coord2pos to)]) coll))
(defn move2move-xy-vec [coll] {:pre [(display-assert (and (vector? coll) (string? (ffirst coll))) coll)]} (into [] (map (fn [[from to]] [(pos2coord from) (pos2coord to)]) coll)))

(defn move-xymap2move-vec [coll] (move-xy2move-vec (map movemap2move coll)))

;(coord2pos [4 6])


;;(move2move-xy ["e5" "e6"])



;; ------------- all possible moves


;(.charAt (name :f) 0)
(defn- is-knight? [ piece]
  (or (= piece :N)
      (= piece :n)))
(defn- is-bishop? [ piece]
  (or (= piece :B)
      (= piece :b)))
(defn- is-queen? [ piece]
  (or (= piece :Q)
      (= piece :q)))
(defn- is-king? [ piece]
  (or (= piece :K)
      (= piece :k)))
(defn- is-rook? [ piece]
  (or (= piece :R)
      (= piece :r)))
(defn- is-pawn? [ piece]
  (or (= piece :P)
      (= piece :p)))



(defprotocol Piece
  (getMoves [this]))

;;(pos2coord "e5")

;;---- pawn moves

(defn- en-passant? [board white? last-move diag x y]
  {:pre [(display-assert (or (and (vector? last-move) (string? (first last-move))) (nil? last-move)) last-move)]}
  (when (not (nil? last-move))
    (let [[op opposite-start-rank] (if white? [+ 1] [- 6])
         [from to] (move2move-xy last-move)
         pawn? (is-pawn? (lookup-xy board to))
         [dx dy] diag
         [fx fy] from
         [tx ty] to
         ]
     (let [ep (and pawn?
                 (= fx tx dx)
                 (= fy opposite-start-rank)
                 (= ty (op fy 2) (op dy 1)))]
       ;(when ep (do (println "possible en-passant move" x y ", last-move" last-move ",board" board)))
       ep))))
;;(en-passant? (en-passant-check-test) white ["c7" "c5"] [2 2] 3 3)
;; => true
;;(en-passant? (en-passant-check-test) white ["c7" "c6"] [2 2] 3 3)
;; => false
;;(en-passant? (en-passant-check-test) white ["c7" "c5"] [2 1] 3 2)
;; => false
;;(en-passant? (en-passant-check-test) white ["c7" "c5"] [2 3] 3 4)
;; => false
;;(en-passant? (en-passant-check-test) white ["c7" "c4"] [2 2] 3 3)
;; => false
;;(en-passant? (en-passant-check-test) white ["e7" "e5"] [4 2] 3 3)
;; => true
;;(en-passant? (en-passant-check-test) white ["c7" "c5"] [4 2] 3 3)
;; => false

;;(en-passant? (en-passant-check-test) white nil [4 2] 3 3)
;; => false


(defn- pawn-moves [board white? last-move x y]
  (let [[op start-rank] (if white? [- 6] [+ 1])
        right-diag [(+ x 1) (op y 1)]
        left-diag [(- x 1) (op y 1)]
        front [x (op y 1)]
        front2 [x (op y 2)]
        moves (vector
               (when (and (pos-xy-within-board? right-diag)
                          (collid-oposite? board white? right-diag))
                 {:move-to right-diag} )
               (when (and (pos-xy-within-board? right-diag)
                          (en-passant? board white? last-move right-diag x y))
                 {:move-to right-diag :en-passant true :taken [(+ x 1) y] :last-move last-move :board board} )
               (when (and (pos-xy-within-board? left-diag)
                          (collid-oposite? board white? left-diag))
                 {:move-to left-diag}  )
               (when (and (pos-xy-within-board? left-diag)
                          (en-passant? board white? last-move left-diag x y))
                 {:move-to left-diag :en-passant true :taken [(- x 1) y] :last-move last-move :board board}  )
               (when (and
                      (pos-xy-within-board? front)
                      (not (collid? board front)))
                 {:move-to front})
               (when (and
                      (pos-xy-within-board? front2)
                      (not (collid? board front))
                      (not (collid? board front2))
                      (= y start-rank))
                 {:move-to front2})

               )
        ]
    (filter (comp not nil?) moves)))

;;(not (= (lookup-xy (initial-board) [2 1]) :.))
;;(pawn-moves (initial-board) false [] 1 1)
;;(map coord2pos (pawn-moves (en-passant-check-test) white ["c7" "c5"] 3 3))
;;(map (fn [a] (meta a)) (pawn-moves (en-passant-check-test) white ["c7" "c5"] 3 3))
;;  (map (fn [a] (meta a)) (pawn-moves (en-passant-check-test) white ["e7" "e5"] 3 3))
;;(->> (pawn-moves (en-passant-check-test) white ["e7" "e5"] 3 3) first meta)

;;(count (pawn-moves (initial-board) white [] 2 2))
;(nothing-between (initial-board) )

(defrecord Pawn [^clojure.lang.PersistentVector board ^String pos ^boolean white? history]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          last-move (last history)
          moves (pawn-moves board white? last-move x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (filter #(no-self-collision? (:move-to %)) (filter #(pos-xy-within-board? (:move-to %)) moves)))))


;;(->> (Pawn. (en-passant-check-test) "d5" white [[(pos2coord "e7") (pos2coord "e5")]]) getMoves first meta)

;;(getMoves (Pawn. (test-board2) "b3" false))
;; => ("c2" "a2" "b2")
;;(getMoves (Pawn. (test-board2) "a7" false))
;; =>("a6" "a5")
;;(getMoves (Pawn. (test-board2) "a6" false))
;; =>("b5" "a5")
;;(getMoves (Pawn. (test-board2) "a6" false))
;; =>("b5" "a5")
;;(getMoves (Pawn. (en-passant-check-test) "d5" true  [["c7" "c5"]]))
;; =>("c6" "d6")

(defn map-move-to2map [moves]
  (map (fn [move] {:move-to move}) moves))

;;---- King moves

(defn- king-moves [board x y]
  (for [a (range -1 2)
        b (range -1 2)
        :when (and
               (or (and (= (+ a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= (- a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= a 0) (not (= b 0)))
                   (and (= b 0) (not (= a 0))))
               (pos-xy-within-board? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (king-moves (initial-board) 2 2))
;(nothing-between (initial-board) )

(defrecord King [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (king-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map-move-to2map (filter no-self-collision? (filter pos-xy-within-board? moves))))))


(getMoves (King. (test-board2) "d3" true))


;; --------- queen moves

(defn- queen-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (and (= (+ a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= (- a b) 0)
                        (not (= a 0))
                        (not (= b 0))
                        )
                   (and (= a 0) (not (= b 0)))
                   (and (= b 0) (not (= a 0))))
               (pos-xy-within-board? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (queen-moves (initial-board) 2 2))
;(nothing-between (initial-board) )

(defrecord Queen [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (queen-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map-move-to2map (filter no-self-collision? (filter pos-xy-within-board? moves))))))

;;(getMoves (Queen. (test-board2) "d3" true))
;;(getMoves (Queen. (could-become-in-check-test) "h5" true))
;;=> ("a3" "b3" "c4" "c3" "d7" "d6" "d5" "d4" "d2" "e4" "e3" "e2" "f5" "f3" "g6" "g3" "h7" "h3")

;;---- Rook

(defn- rook-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (and (= a 0) (not (= b 0))) (and (= b 0) (not (= a 0))))
               (pos-xy-within-board? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (rook-moves (initial-board) 2 2))
;(nothing-between (initial-board) )

(defrecord Rook [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (rook-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map-move-to2map (filter no-self-collision? (filter pos-xy-within-board? moves))))))


(getMoves (Rook. (test-board2) "c2" true))

;;---- bishop


(defn- bishop-moves [board x y]
  (for [a (range -7 8)
        b (range -7 8)
        :when (and
               (or (= (+ a b) 0) (= (- a b) 0))
               (not (= a 0))
               (not (= b 0))
               (pos-xy-within-board? [(+ a x) (+ b y)])
               ;(not (collid-self? board true [x y]))
               (nothing-between board [(+ a x) (+ b y)] [x y])
               )] [(+ a x) (+ b y)]))

(count (bishop-moves (initial-board) 1 2))
;(nothing-between (initial-board) )

(defrecord Bishop [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          moves (bishop-moves board x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map-move-to2map (filter no-self-collision? (filter pos-xy-within-board? moves))))))


(getMoves (Bishop. (initial-board) "a3" true))
;;("b4" "c5" "d6" "e7")
(getMoves (Bishop. (test-board2) "c1" true))
;;=> ("a3" "b2" "d2" "e3" "f4" "g5" "h6")
(getMoves (Bishop. (test-board2) "a5" true))
;;=> ("b6" "b4" "c7")
(getMoves (Bishop. (test-board2) "e2" true))
;;("d3" "f3" "g4" "h5")


;; ---- knight stuff
(defn- knight-moves [x y]
  #{[(+ x 2) (+ y 1)]
    [(+ x 1) (+ y 2)]
    [(- x 1) (+ y 2)]
    [(- x 2) (+ y 1)]
    [(- x 2) (- y 1)]
    [(- x 1) (- y 2)]
    [(+ x 1) (- y 2)]
    [(+ x 2) (- y 1)]})

;;(knight-moves 0 0)

(defrecord Knight [^PersistentVector board ^String pos ^boolean white?]
  Piece
  (getMoves [this]
    (let [[x y] (pos2coord pos)
          kmoves (knight-moves x y)
          no-self-collision? (comp not (partial collid-self? board white?))]
      (map-move-to2map (filter no-self-collision? (filter pos-xy-within-board? kmoves))))))

;;(getMoves (Knight. (initial-board) "g1" true))
;; f3 g3

(defn- one-color [^PersistentVector board ^Boolean white?]
  (let [color? (if white? is-white? is-black?)]
    (map #(if (color? %) % :. ) board)))

(one-color (initial-board) false)

(defn convert2obj [^PersistentVector board history pos-xy]
  (let [piece (lookup-xy board pos-xy)
        color (is-white? piece)
        pos (coord2pos pos-xy)]
    (cond
     (is-knight? piece) (Knight. board pos color)
     (is-bishop? piece) (Bishop. board pos color)
     (is-queen? piece) (Queen. board pos color)
     (is-king? piece) (King. board pos color)
     (is-rook? piece) (Rook. board pos color)
     (is-pawn? piece) (Pawn. board pos color history))))

;;(convert2obj (initial-board) [] (pos2coord "h1"))

;;---- move validation

(defn possible-moves [^PersistentVector board history pos-xy]
  (getMoves (convert2obj board history pos-xy)))

(defn all-possible-moves [^PersistentVector board ^Boolean is-player1-turn history]
  (->> (one-color board is-player1-turn)
       board2xy-map-piece
       (mapcat
        (fn [[pos-xy _]]
          (map
           (fn [move] [pos-xy move])
           (possible-moves board history pos-xy))))))

;;(map (fn [[a b]] [(meta a) (meta b)]) (all-possible-moves (en-passant-check-test) true false [[[2 1] [2 3]]]))

;;(filter #(is-piece? %))
;;(map (fn [[pos c]] (possible-moves board pos)))
;(all-possible-moves (initial-board) true false [])
;(count (all-possible-moves (initial-board) true false []))

(defn king-pos [board king-white?]
  (let [king (if king-white? :K :k)]
    (->> ((->>  (group-by #(second %) (board2xy-map-piece board)) (into {})) king) ffirst)))

(king-pos (check-mate-test) false)
;; => [4 0]

(defn check? [board white-king? history]
  (let [opposite-moves-xy2map (all-possible-moves board (not white-king?) history)
        king (king-pos board white-king?)]
    (not (not (some #(= % king) (map #(:move-to (second %)) opposite-moves-xy2map))))))

;;(check? (check-mate-test) false false [])
;;(check? (initial-board) false false [])


;; -------------- rendering

;;(def ^:const board (vec (range 8)))


;;(c2dto1d [1 1])
;;(c1dto2d 63)


;;----- change state of board (make moves)

(defn apply-move [^PersistentVector board ^PersistentVector move] ;; xy or xymap
  (let [[from to] move
        en-passant (:en-passant to)
        taken (:taken to)
        b (:board to)
        real-to (if en-passant (:move-to to) to)
        piece (lookup-xy board from)
        new-board (-> (assoc board (apply index-xy from) :.)
                      (assoc (apply index-xy real-to) piece))]
    (if en-passant
      (assoc new-board (apply index-xy taken) :.)
      new-board)))

;;(display-board (apply-move (initial-board) ["b2" "b3"]))

(defn all-possible-moves-with-in-check [board is-player1-turn history]
  (let [possible-moves-xy2map (all-possible-moves board is-player1-turn history)
        f (fn [[from {to :move-to}]] (let [possible-new-board (apply-move board [from to])]
                      (not (check? possible-new-board is-player1-turn history))))]
    (filter f possible-moves-xy2map)))



;;(map (fn [[a b]] [(meta a) (meta b)]) (all-possible-moves-with-in-check (en-passant-check-test) true false [[[2 1] [2 3]]]))

;;(all-possible-moves-with-in-check (check-mate-test) false false [])
;;(all-possible-moves-with-in-check (in-check-test) false false [])
;; => ([[4 1] [3 2]])
;(filter (fn [[from to]] (= from "f7")) (all-possible-moves-with-in-check (could-become-in-check-test) false false))


(defn- normalize [move]
  (if (and (vector? move)
           (keyword? (first move))
           (keyword? (second move)))
    (let [[f t] move]
      [(name f) (name t)])
    move))


;; todo: catch any exception
;; todo: check that any none valid input returns nil
(defn is-move-valid? [^PersistentVector board ^Boolean is-player1-turn ^PersistentVector history ^PersistentVector move]
  {:pre [(display-assert (and (vector? history) (or (nil? (first history)) (string?  (ffirst history)))) history)]}
  (let [norm-move (normalize move)]
    (if (and
         (vector? norm-move)
         (string? (first norm-move))
         (= (count (first norm-move)) 2))
      (let [moves-xy2map (all-possible-moves-with-in-check board is-player1-turn history)]
        (not (not (some #(= (move2move-xy norm-move) (movemap2move %)) moves-xy2map))))
      (do (println "move" move "is not formatted correctly")  false))))


(defn move-en-passant [^PersistentVector board ^Boolean is-player1-turn ^Boolean castle? ^PersistentVector history ^PersistentVector move]
  (let [moves-xy-map (all-possible-moves-with-in-check board is-player1-turn history)]
    (first (filter #(let [[from {to :move-to :as tomap}] %] (and (= move [from to])
                          (:en-passant tomap)))
                   moves-xy-map))))
;;(play-scenario [["e2" "e4"] ["d7" "d5"] ["e4" "d5"] ["e7" "e5"] ["d5" "e6"] ["d5" "e6"]])

;;(is-move-valid? (en-passant-check-test) white false (move2move-xy-vec [["c7" "c5"]]) (move2move-xy ["d5" "c6"]))
;;(move-en-passant (en-passant-check-test) white false (move2move-xy-vec [["c7" "c5"]]) (move2move-xy ["d5" "c6"]))

;;(all-possible-moves-with-in-check (en-passant-check-test) white false (move2move-xy-vec [["c7" "c5"]]) )
;;(is-move-valid? (en-passant-check-test) true false [] ["d5" "c4"])
;; => true


(defn check-mate? [^PersistentVector board ^Boolean is-player1-turn ^PersistentVector history]
  (->> (all-possible-moves-with-in-check board is-player1-turn history) count zero?))
;(check-mate? (check-mate-test) false false [])
;(check-mate? (in-check-test) false false [])


(defn forfeit [is-player1-turn]
  (if is-player1-turn [0 1] [1 0]))
(def opposite-color-wins forfeit)

(defn stacktrace [t]
  (with-out-str (clojure.stacktrace/print-stack-trace t)))

(defn get-playing-id [{:keys [id1 id2 is-player1-turn]
                       :or { id1 "undefined"
                            id2 "undefined"}}]
  (if is-player1-turn
    id1
    id2))
(defn get-playing-f-state [{:keys [state-f1 state-f2 is-player1-turn]}]
  (if is-player1-turn
    state-f1
    state-f2))
(defn get-playing-f [{:keys [f1 f2 is-player1-turn]}]
  (if is-player1-turn
    f1
    f2))

;;(get-playing-id { })

(defn execute [f context]
  (try (f context )
       (catch java.util.concurrent.TimeoutException t
         (do (println "caught TimeoutException because chess player function did not return in time" t)
             {:result :too-slow-to-move :exception (.toString t) :stacktrace (stacktrace t)}))
       (catch java.lang.SecurityException t
         (do (println "caught security exception inside chess player function " (.getMessage t))
             {:result :security-exception :exception (.toString t) :stacktrace (stacktrace t)}))
       (catch Throwable t
         (do (println "caught exception inside chess player function" (.getMessage t) ", by" (:id context))
             {:result :caught-exception :exception (.toString t) :stacktrace (stacktrace t)}))
       ))
(defn execute-no-catch [f game-context]
  (f game-context )
  )

(defn parse-f-return [ret]
  (if  (map? ret)
    ret
    {:move ret}))

(defn log [channel res]
  (do
    (when channel
      (println "about to send IGU to channel")
      (go (>! channel res))
      (println "in-game-update sent to channel"))
    res))

(macroexpand '(display-assert (and (some? board) (some? f1) (some? f2) (some? history)) board f1 f2 history)
             )
(quote board f1 f2 history)

;;(display-board (apply-move-safe (initial-board) true false ["a2" "b3"]))
(defn play-game-step [{:keys [board f1 f2 id1 id2 is-player1-turn history state-f1 state-f2 iteration channel game-id] :as game-context}]
  {:pre [(display-assert (and (some? board) (some? history)) board history)]}
  (cond (or (nil? f1) (nil? f2)) nil
   (check-mate? board is-player1-turn history)
   (do
        (println "check-mate!")
        (vector true {:score (opposite-color-wins is-player1-turn) :history history :board board :result :check-mate}))
   :else (let [new-iteration (if (nil? iteration) 1 (inc iteration))
            in-check? (check? board (not is-player1-turn) history)
            valid-moves (into [] (move-xymap2move-vec (all-possible-moves-with-in-check board is-player1-turn history)))
            f-return (execute (get-playing-f game-context) {:board board
                                                            :is-player1-turn is-player1-turn
                                                            :valid-moves valid-moves
                                                            :in-check? in-check?
                                                            :history history
                                                            :state (get-playing-f-state game-context)
                                                            :id (get-playing-id game-context)})
            {result :result move :move new-state :state exception :exception :as f-result} (parse-f-return f-return)]
        (cond (> new-iteration 500)         (vector true {:score [1/2 1/2] :history (conj history new-iteration) :board board :result :draw-by-number-of-iteration})
              (not (nil? exception)) (vector true (merge {:score (forfeit is-player1-turn) :history (conj history :exception ) :board board} f-result))
              :else (let [valid? (is-move-valid? board is-player1-turn history move)
                norm-move (normalize move)
                new-history (conj history norm-move)]
            (if (not valid?)
              (vector true {:score (forfeit is-player1-turn) :history new-history :board board :result :invalid-move})
              (let [
                   move-xy (move2move-xy  norm-move)
                   en-passant-move-xymap (move-en-passant board is-player1-turn false history move-xy)
                   real-move (if en-passant-move-xymap en-passant-move-xymap move-xy)]
               (vector false (log channel (merge
                                    {:board (apply-move board real-move)
                                     :f1 f1 :f2 f2 :id1 id1 :id2 id2
                                     :msg-type :in-game-update
                                     :game-id game-id
                                     :is-player1-turn (not is-player1-turn) :history new-history :channel channel :iteration new-iteration}
                                    (if is-player1-turn
                                      {:state-f1 new-state :state-f2 state-f2}
                                      {:state-f1 state-f1 :state-f2 new-state})))))
             ))))))

(defn replay-game-step [{:keys [board f1 f2 id1 id2 is-player1-turn history state-f1 state-f2 iteration] :as game-context}]
  (let [new-iteration (if (nil? iteration) 1 (inc iteration))
        f-return (execute (get-playing-f game-context) {
                                                        :is-player1-turn is-player1-turn
                                                        :history history
                                                        :state (get-playing-f-state game-context)
                                                        :id (get-playing-id game-context)})
        {result :result move :move new-state :state exception :exception :as f-result} (parse-f-return f-return)
        norm-move (normalize move)
        new-history (conj history norm-move)
        move-xy (move2move-xy  norm-move)
        en-passant-move-xymap (move-en-passant board is-player1-turn false history move-xy)
        real-move (if en-passant-move-xymap en-passant-move-xymap move-xy)]
    (vector false (merge
                   {:board (apply-move board real-move)
                    :f1 f1 :f2 f2 :id1 id1 :id2 id2
                    :msg-type :in-game-update
                    :is-player1-turn (not is-player1-turn) :history new-history :iteration new-iteration}
                   (if is-player1-turn
                     {:state-f1 new-state :state-f2 state-f2}
                     {:state-f1 state-f1 :state-f2 new-state})))
    ))

(def game-step (game-step-monad-wrap play-game-step))

(def replay-game-step-monad (game-step-monad-wrap replay-game-step))



;; this function is central to the chess engine
(defn game-seq [monadic-step init-state]
  (unfold
   monadic-step
   init-state
   ))


(comment

(def f1-form-old
  '(fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history s :state}]
     (do
       (let [move-seq (if (nil? s)
                       (vector ["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"] )
                       s)]
        {:move (first move-seq) :state (into [] (next move-seq))})))
)
(def f1-form
  '(fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history s :state}]
     (do
       ;;(println "s=" s)
       (let [i (if (nil? s)
                       0
                       s)]
         {:move (get (vector ["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"] ) i) :state (inc i)})))
)

(def f2-form
  '(fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history option-state :state}]
     (let [b board
           move-seq (if (nil? option-state)
                      (list ["e7" "e5"] ["d7" "d6"] ["b8" "c6"] ["e8" "e7"])
                      option-state)]
       {:move (first move-seq) :state (into [] (next move-seq))}))
)


 (second (game-seq-old play-game-step {:board (initial-board) :is-player1-turn true :history [] :f1 f1 :f2 f2}))
 ((comp play-game-step second) [false {:board (initial-board) :is-player1-turn true :history [] :f1 f1 :f2 f2}])

 (take 10 (game-seq play-game-step {:board (initial-board) :is-player1-turn true :history [] :f1 f1 :f2 f2}))
 (take 10 (game-seq game-step {:board (initial-board) :is-player1-turn true :history [] :f1 f1 :f2 f2}))

 (count (game-step {:board (initial-board) :is-player1-turn true :history [] :f1 f1 :f2 f2}))
 (replay-game-step-monad {:board (initial-board) :is-player1-turn true :history [] :f1 f1 :f2 f2})
 (replay-game-step-monad {:board (initial-board) :is-player1-turn true :history [] :result []})
 (count (game-step {:board (initial-board) :is-player1-turn true :history [] }))
 (count (play-game-step {:board (initial-board) :is-player1-turn true :history [] }))

 (first (game-seq play-game-step {:board (initial-board) :is-player1-turn true :history [] :f1 random-f-no-print :f2 random-f-no-print}))

 (take 3 (game-seq game-step {:board (initial-board) :is-player1-turn true :history [] :f1 (sb f1-form) :f2 f2}))

(board-seq [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]])


 )


;;(def m-game-seq (memoize game-seq))
(def m-game-seq game-seq)


;;(take 10 (play-scenario-seq game-step  [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]]))

;;for testing only
(defn play-game-rec [game-init]
  (loop [state (merge game-init {:game-id (str (java.util.UUID/randomUUID))})]
    (let [[v s] (play-game-step state)]
      (if v
        s
        (recur s))))
  )
;; => [score history last-board invalid-move? check-mate?]
;;example => [[1 0] [["e2" "e4"] ["e7" "e5"]] [:. :. :. :k :. ....]]

;;(play-game {})



(defn- every-nth [coll n]
  (map (fn [[i e]] e) (filter (fn [[i e]] (zero? (mod i n))) (map-indexed (fn [i e] [i e]) coll))))

(defn- create-fn [moves]
  (fn [{b :board c :is-player1-turn ic :in-check? h :history s :state}]
      (let [move-seq (if (nil? s)
                       moves
                       s)]
        {:move (first move-seq) :state (next move-seq)})))

(defn create-fns-from-scenario [moves]
  (let [white-moves (every-nth moves 2)
        black-moves (every-nth (drop 1 moves) 2)]
    [(create-fn white-moves)
     (create-fn black-moves)]))




;; (defn play-scenario [scenario] (let [[f1 f2] (create-fns-from-scenario scenario)]
;;                                  (let [result (play-game {:board (initial-board) :f1 f1 :f2 f2})]
;;                                    result)))


(defn play-scenario-seq [step scenario & [state]] (let [[f1 f2] (create-fns-from-scenario scenario)]
                                                  (let [result (play-game-seq step
                                                                              (merge
                                                                               {:board (initial-board) :history [] :f1 f1 :f2 f2}
                                                                               state))]
                                   (take (count scenario) result))))

;; (defn play-scenario-seq [scenario]
;;   (play-scenario-with-step-seq game-step scenario))


(defn play-game [game-init]
  (seq-result
   (play-game-seq game-step (merge game-init {:board (initial-board) :history []}))))


;;(play-game-seq game-step (merge {:board (initial-board) :f1  (fn [_] ["e8" "A4"]) :f2 (fn [_] ["e8" "A4"])} {:board (initial-board) :history []}))
;;(game-seq game-step (merge {:board (initial-board) :f1  (fn [_] ["e8" "A4"]) :f2 (fn [_] ["e8" "A4"])} {:board (initial-board) :history []}))
;;(game-step (merge {:board (initial-board) :f1  (fn [_] ["e8" "A4"]) :f2 (fn [_] ["e8" "A4"])} {:board (initial-board) :history []}))


(defn play-scenario [scenario & [state]]
  (seq-result
   (play-scenario-seq game-step scenario state)))

(comment
  (take 10 (play-scenario  [["e2" "e4"] ["e7" "e5"]
                    ["d1" "h5"] ["d7" "d6"]
                    ["f1" "c4"] ["b8" "c6"]
                    ["h5" "f7"] ["e8" "e7"]]))
  (count (play-scenario-seq game-step [["e2" "e4"] ["e7" "e5"]
                                 ["d1" "h5"] ["d7" "d6"]
                                 ["f1" "c4"] ["b8" "c6"]
                                 ["h5" "f7"] ["e8" "e7"]]))
  )
;; (defn board-seq [white-moves black-moves]
;;   (let [f1 (create-fn white-moves)
;;         f2 (create-fn black-moves)]
;;                                  (let [result (play-game {:board (initial-board) :f1 f1 :f2 f2})]
;;                                    result)))

;; (board-seq  [["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"]]
;;             [["e7" "e5"] ["d7" "d6"] ["b8" "c6"] ["e8" "e7"]])

(defn append-val [key old-vals val]
  (fn [s]
    (let [;old-vals (get s key [])
	  new-s   (assoc s key (conj old-vals val))]
      [old-vals new-s])))

;;(def m-replay-game-step (memoize replay-game-step))
(def m-replay-game-step replay-game-step)

(defn board-seq [moves]
  (->>
   (play-scenario-seq
    replay-game-step-monad
    moves) (map second) (map :board) (take (count moves)) (cons (initial-board))))

(comment
  (->>
   (play-scenario-seq
    (domonad state-m
             [bh (fetch-val :board-history)
              a game-step
              b (fetch-val :board)
              c (append-val :board-history bh b)]
             a)
    [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]] ) (map second) (map :board))
  (board-seq [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]] )
  (take 8 (play-scenario-seq
    replay-game-step-monad
    [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]] ))


  (profile (nth (board-seq [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]] ) 8))
  (macroexpand '(profile (nth (board-seq [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]] ) 8)))


  (->> )

  )

;;(first (play-scenario-seq game-step [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]]))
;; => check-mate
;;(play-scenario  [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "g6"] ["e8" "e7"]])
;; => invalid move

(defn interactive-f [{board :board am-i-white? :is-player1-turn valid-moves :valid-moves ic :in-check? h :history s :state}]
  (do
    (display-board board)
    (println (if am-i-white? "white: " "black: "))
    (println "valid moves:" valid-moves)
    (println "enter next move (in format [\"a2\" \"a3\"]):")
    (let [move (read-string (read-line))]

     move)))

;;; test functions

(def f1
  (fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history s :state}]
     (let [move-seq (if (nil? s)
                      (list ["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"])
                      s)]
       {:move (first move-seq) :state (next move-seq)}))
)

(def f2
  (fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history option-state :state}]
     (let [b board
           move-seq (if (nil? option-state)
                      (list ["e7" "e5"] ["d7" "d6"] ["b8" "c6"] ["e8" "e7"])
                      option-state)]
       {:move (first move-seq) :state (next move-seq)}))
)

(defn random-f [{board :board am-i-white? :is-player1-turn valid-moves :valid-moves ic :in-check? h :history s :state}]
  (let [v (into [] valid-moves)
        iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
    (display-board board)
    (println (if am-i-white? "white: " "black: "))
    (println "valid moves:" valid-moves)
    (println "iteration:" iteration)
    (let [move (rand-int (count valid-moves))]
      (println "choosen move:" (get v move))
      {:move (get v move) :state iteration})) )
(defn random-f-no-print [{board :board am-i-white? :is-player1-turn valid-moves :valid-moves ic :in-check? h :history s :state}]
  (let [v (into [] valid-moves)
        iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
    (display-board board)
    (let [move (rand-int (count valid-moves))]
      {:move (get v move) :state iteration})) )
(def random-f-form-print '(fn random-f [{board :board am-i-white? :is-player1-turn valid-moves :valid-moves ic :in-check? h :history s :state}]
   (let [v (into [] valid-moves)
         iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
     ;;(display-board board)
     (println (if am-i-white? "white: " "black: "))
     (println "valid moves:" valid-moves)
     (println "iteration:" iteration)
     (let [move (rand-int (count valid-moves))]
       (println "choosen move:" (get v move))
       {:move (get v move) :state iteration})) ))


(def random-f-form '(fn random-f [{board :board, am-i-white? :is-player1-turn, valid-moves :valid-moves, ic :in-check?, h :history, s :state}]
                      (let [v (into [] valid-moves)
                            iteration (if (nil? s) (+ 1 (if am-i-white? 0 1)) (+ 2 s))]
                        (let [move (rand-int (count valid-moves))]
                          {:move (get v move), :state iteration}))))



;; (play-game (initial-board) interactive-f f2)
;; (play-game (initial-board) interactive-f random-f)
;;(rand-int 42)

(defn wrapper-display-f [f]
  (fn [{board :board am-i-white? :is-player1-turn valid-moves :valid-moves ic :in-check? h :history s :state :as game-context}]
    (do
     (display-board board)
     (println (if am-i-white? "white: " "black: "))
     (println "valid moves:" valid-moves)
     (let [{:keys [move] :as res} (f game-context)]
       (println "choosen move:" move)
       res))))

(defn trace-game-play
  ([{:keys [white black]}]
     (let [rf2 (if black (wrapper-display-f black) interactive-f)
           rf1 (if white (wrapper-display-f white) interactive-f)
           result (play-game {:board (initial-board) :f1 rf1 :f2 rf2 :id1 "daredevil" :id2 "wonderboy"})]
       (println result)
       ))
  )

(defn mini-tournement []
  (let [result (play-game {:board (initial-board) :f1 random-f :f2 random-f :id1 "daredevil" :id2 "wonderboy"})]
   (println result)
   (recur)))

(defn sb
  ([] (sandbox (conj secure-tester (blacklist-objects [java.lang.Thread])) :timeout 40000))
  ([form] (fn [arg] ((sb) (list form arg))))
  ([form timeout] (fn [arg] ((sandbox (conj secure-tester (blacklist-objects [java.lang.Thread])) :timeout timeout) (list form arg)))))

(comment

(def arg {:history [["e2" "e4"] ["e7" "e5"]], :iteration 2, :state-f1 '(["d1" "h5"] ["f1" "c4"] ["h5" "f7"]), :f1 (sb f1-form), :f2 f2, :board [:r :n :b :q :k :b :n :r :p :p :p :p :. :p :p :p :. :. :. :. :. :. :. :. :. :. :. :. :p :. :. :. :. :. :. :. :P :. :. :. :. :. :. :. :. :. :. :. :P :P :P :P :. :P :P :P :R :N :B :Q :K :B :N :R], :is-player1-turn true, :game-id nil, :channel nil, :msg-type :in-game-update, :id1 nil, :id2 nil, :state-f2 '(["d7" "d6"] ["b8" "c6"] ["e8" "e7"])})
(def arg {:history [["e2" "e4"] ["e7" "e5"]], :iteration 2, :state-f1 '(["d1" "h5"] ["f1" "c4"] ["h5" "f7"]), :f1 f1, :f2 f2, :board [:r :n :b :q :k :b :n :r :p :p :p :p :. :p :p :p :. :. :. :. :. :. :. :. :. :. :. :. :p :. :. :. :. :. :. :. :P :. :. :. :. :. :. :. :. :. :. :. :P :P :P :P :. :P :P :P :R :N :B :Q :K :B :N :R], :is-player1-turn true, :game-id nil, :channel nil, :msg-type :in-game-update, :id1 nil, :id2 nil, :state-f2 '(["d7" "d6"] ["b8" "c6"] ["e8" "e7"])})


((fn [{board :board, am-i-white? :is-player1-turn, ic :in-check?, h :history, s :state}]
   (let [move-seq (if (nil? s)
                    (list ["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"])
                    s)]
     {:move (first move-seq), :state (next move-seq)})) arg)

(game-step arg)


(def f-arg {:board [:r :n :b :q :k :b :n :r :p :p :p :p :. :p :p :p :. :. :. :. :. :. :. :. :. :. :. :. :p :. :. :. :. :. :. :. :P :. :. :. :. :. :. :. :. :. :. :. :P :P :P :P :. :P :P :P :R :N :B :Q :K :B :N :R], :is-player1-turn true, :valid-moves [["h2" "h3"] ["h2" "h4"] ["g2" "g3"] ["g2" "g4"] ["f2" "f3"] ["f2" "f4"] ["g1" "f3"] ["g1" "e2"] ["g1" "h3"] ["f1" "a6"] ["f1" "b5"] ["f1" "c4"] ["f1" "d3"] ["f1" "e2"] ["d2" "d3"] ["d2" "d4"] ["e1" "e2"] ["c2" "c3"] ["c2" "c4"] ["d1" "e2"] ["d1" "f3"] ["d1" "g4"] ["d1" "h5"] ["b2" "b3"] ["b2" "b4"] ["a2" "a3"] ["a2" "a4"] ["b1" "c3"] ["b1" "a3"]], :in-check? false, :history [["e2" "e4"] ["e7" "e5"]], :state [["d1" "h5"] ["f1" "c4"] ["h5" "f7"],] :id nil})

((sb random-f-form) f-arg)
((sb f1-form-old) f-arg)
(f1 f-arg)

)




;; (take 2 (game-seq game-step {:board (initial-board) :is-player1-turn true :history [] :f1 (sb f1-form) :f2 f2}))
;; (take 3 (game-seq game-step {:board (initial-board) :is-player1-turn true :history [] :f1 (sb f1-form-old) :f2 f2}))

(defn sand-boxed-mini-tournement []
  (let [result (play-game {:board (initial-board) :f1 (fn [in] ((sb) (list random-f-form in))) :f2 (sb random-f-form-print) :id1 "daredevil" :id2 "wonderboy"})]
   (println result)
   (recur)))

;;(sand-boxed-mini-tournement)

;;((fn [in] ((sb) (list random-f-form in))) {:valid-moves [["e5" "e7"]]})
;;((sb) (list random-f-form {}))

(defn -main []
 (mini-tournement))


(defmacro with-time-assoced
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the assoced map with :time -> created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)
         oldout# *out*]
     (binding [*out* s#]
       (assoc (time (binding [*out* oldout#] ~@body)) :time (str s#)))))
