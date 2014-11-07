(ns zone.lambda.game.chess.core-test
  (:require [clojure.test :refer :all]
            [zone.lambda.game.board :as board :refer [file-component is-white? is-black? is-piece?
                                                      ]]
            [zone.lambda.game.chess.core :refer :all]))

(def rank-component (partial board/rank-component column-nb))


(def en-passant-check-board ;; it's white's turn
  [:r :. :b :q :k :b :n :r
   :p :p :. :. :. :p :p :p
   :. :. :. :. :. :. :. :.
   :n :. :p :P :p :. :. :Q
   :. :. :B :. :. :. :. :.
   :. :. :. :. :. :. :. :N
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :. :R])

(def en-passant-check-board2 ;; it's white's turn
  [:r :. :b :q :k :b :n :r
   :p :p :. :. :. :p :p :p
   :. :. :p :. :. :. :. :.
   :n :. :. :P :p :. :. :Q
   :. :. :B :. :. :. :. :.
   :. :. :. :. :. :. :. :N
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :. :R])

(def castle-check-board ;; it's white's turn
  [:r :. :b :q :k :b :n :r
   :p :p :. :. :. :p :p :p
   :. :. :. :. :. :. :. :.
   :n :. :p :P :p :. :. :Q
   :. :. :B :. :. :. :. :.
   :. :. :. :. :. :. :. :N
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :. :R])

(def could-become-in-check-board ;; it's black's turn
  [:r :. :b :q :k :b :n :r
   :p :p :p :p :. :p :p :p
   :. :. :n :. :. :. :. :.
   :. :. :. :. :p :. :. :Q
   :. :. :B :. :P :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :N :R])


(def in-check-board  ;; it's blacks turn, king is in check.
  [:r :. :b :q :. :b :n :r
   :p :p :p :p :k :Q :p :p
   :. :. :n :. :. :p :. :.
   :. :. :. :. :p :. :. :.
   :. :. :B :. :P :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :N :R])


(def check-mate-board  ;; it's blacks turn, king is in check. no move will save him => check mate
  [:r :. :b :q :k :b :n :r
   :p :p :p :p :. :Q :p :p
   :. :. :n :. :. :p :. :.
   :. :. :. :. :p :. :. :.
   :. :. :B :. :P :. :. :.
   :. :. :. :. :. :. :. :.
   :P :P :P :P :. :P :P :P
   :R :N :B :. :K :. :N :R])

(def promotion-check-board ;; it's white's turn
  [:. :. :. :. :k :. :. :.
   :. :P :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :K :. :. :.])

(def promotion-result-board ;; it's white's turn
  [:. :Q :. :. :k :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :.
   :. :. :. :. :K :. :. :.])


(deftest test-promotion
  (testing ""
    (is
     (=
      (:board (play-scenario  [["b7" "b8"]] {:board promotion-check-board}))
      promotion-result-board))))

(deftest test-no-promotion-when-not-a-pawn
  (testing "double checking if not-pawns won't be promoted")
  (is
   (=
    (:board (play-scenario [["b7" "b8"]] {:board  [:. :. :. :. :k :. :. :.
                                                   :. :K :. :. :. :. :. :.
                                                   :. :. :. :. :. :. :. :.
                                                   :. :. :. :. :. :. :. :.
                                                   :. :. :. :. :. :. :. :.
                                                   :. :. :. :. :. :. :. :.
                                                   :. :. :. :. :. :. :. :.
                                                   :. :. :. :. :R :. :. :.]}))
     [:. :K :. :. :k :. :. :.
      :. :. :. :. :. :. :. :.
      :. :. :. :. :. :. :. :.
      :. :. :. :. :. :. :. :.
      :. :. :. :. :. :. :. :.
      :. :. :. :. :. :. :. :.
      :. :. :. :. :. :. :. :.
      :. :. :. :. :R :. :. :.])))

(deftest test-filing

  (testing ""
    (is (= (file-component \b)
           1))))

(deftest test-rank
  (testing ""
    (is (= (rank-component \1)
           56))))

(deftest test-pos2coord1
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "h3")
           [7 5]))))
(deftest test-pos2coord2
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "a1")
           [0 7]))))
(deftest test-pos2coord3
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "a7")
           [0 1]))))
(deftest test-pos2coord4
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "e5")
           [4 3]))))
(deftest test-pos2coord5
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "h8")
           [7 0]))))
(deftest test-pos2coord6
  (testing "convertion from algebraic notation to xy coordinates"
    (is (= (pos2coord "h1")
           [7 7]))))

(deftest test-coord2pos1
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [7 7])
           "h1"))))
(deftest test-coord2pos2
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [0 7])
           "a1"))))
(deftest test-coord2pos3
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [7 0])
           "h8"))))
(deftest test-coord2pos4
  (testing "convertion xy coordinates to algebraic notation"
    (is (= (coord2pos [4 3])
           "e5"))))

(deftest test-color
  (testing "test color"
    (is (= (is-white? :K)
           true))))
(deftest test-color2
  (testing "test color"
    (is (= (is-white? :k)
           false))))
(deftest test-color3
  (testing "test color"
    (is (= (is-white? :.)
           false))))
(deftest test-color4
  (testing "test color"
    (is (= (is-black? :K)
           false))))
(deftest test-color5
  (testing "test color"
    (is (= (is-black? :k)
           true))))
(deftest test-color6
  (testing "test color"
    (is (= (is-black? :.)
           false))))

(deftest test-presence-of-piece
  (testing "test absence of piece"
    (is (= (is-piece? :.)
           false))))
(deftest test-presence-of-piece2
  (testing "test presence of piece - with a white piece"
    (is (= (is-piece? :P)
           true))))
(deftest test-presence-of-piece3
  (testing "test presence of piece - with a black piece"
    (is (= (is-piece? :k)
           true))))

(deftest test-opposite-collision
  (testing "test collision - white piece is about to move onto c1"
    (is (= (collid-oposite? (initial-board) white-turn [2 7])
           false))))
(deftest test-opposite-collision2
  (testing "test collision - black piece is about to move onto c1"
    (is (= (collid-oposite? (initial-board) black-turn [2 7])
           true))))
(deftest test-self-collision
  (testing "test collision - white piece is about to move onto c8"
    (is (= (collid-self? (initial-board) white-turn [2 0])
           false))))
(deftest test-self-collision1
  (testing "test collision - black piece is about to move onto c8"
    (is (= (collid-self? (initial-board) black-turn [2 0])
           true))))
(deftest test-self-collision2
  (testing "test collision - black piece is about to move onto c3"
    (is (= (collid-self? (initial-board) black-turn [2 2])
           false))))


(deftest test-self-collision3
  (testing "test collision - white piece is about to move onto c1"
    (is (= (collid-self? (initial-board) white-turn [2 7])
           true))))
(deftest test-self-collision4
  (testing "test collision - black piece is about to move onto c1"
    (is (= (collid-self? (initial-board) black-turn [2 7])
           false))))
(deftest test-self-collision5
  (testing "test collision - white piece is about to move onto c1"
    (is (= (collid-self? (initial-board) white-turn [2 7])
           true))))
(deftest test-self-collision6
  (testing "test collision - black piece is about to move onto c1"
    (is (= (collid-self? (initial-board) black-turn [2 7])
           false))))
(deftest test-self-collision7
  (testing "test collision - black piece is about to move onto c3"
    (is (= (collid-self? (initial-board) black-turn [2 5])
           false))))

(deftest test-any-collision
  (testing "test collision - piece is about to move onto"
    (is (= (collid? (initial-board) [2 5])
           false))))
(deftest test-any-collision2
  (testing "test collision - piece is about to move onto"
    (is (= (collid? (initial-board) [2 6])
           true))))
(deftest test-any-collision3
  (testing "test collision - piece is about to move onto"
    (is (= (collid? (initial-board) [2 1])
           true))))

(deftest test-lookup-xy
  (testing "return piece Character from an algebraic notation given a board state - White Rook"
    (is (= (lookup-xy (initial-board) [0 7])
           :R))))


(deftest test-lookup
  (testing "return piece Character from an algebraic notation given a board state - White Rook"
    (is (= (lookup (initial-board) "a1")
           :R))))
(deftest test-lookup2
  (testing "White Queen"
    (is (= (lookup (initial-board) "d1")
           :Q))))
(deftest test-lookup3
  (testing "White Pawn"
    (is (= (lookup (initial-board) "d2")
           :P))))
(deftest test-lookup4
  (testing "Black King "
    (is (= (lookup (initial-board) "e8")
           :k))))
(deftest test-lookup5
  (testing "empty square"
    (is (= (lookup (initial-board) "e3")
           :.))))

(deftest test-find-king-position
  (testing "find black king"
    (is (= (king-pos check-mate-board black))
        "e8")))

(deftest test-all-possible-move-init
  (testing "that all the 20 possibilities (8 pawn x2 moves + 2 knights x2 moves) are found for the first move"
    (is (= (into #{} (move-xymap2move-vec (all-possible-moves (initial-board) white [])))
           (into #{} '(["h2" "h3"] ["h2" "h4"] ["g2" "g3"] ["g2" "g4"] ["f2" "f3"] ["f2" "f4"] ["g1" "f3"] ["g1" "h3"] ["e2" "e3"] ["e2" "e4"] ["d2" "d3"] ["d2" "d4"] ["c2" "c3"] ["c2" "c4"] ["b2" "b3"] ["b2" "b4"] ["a2" "a3"] ["a2" "a4"] ["b1" "c3"] ["b1" "a3"]))))))

(deftest test-all-possible-move-with-in-check-init
  (testing "that all the 20 possibilities (8 pawn x2 moves + 2 knights x2 moves) are found for the first move"
    (is (= (into #{}  (move-xymap2move-vec (all-possible-moves-with-in-check (initial-board) white [])))
           (into #{} '(["h2" "h3"] ["h2" "h4"] ["g2" "g3"] ["g2" "g4"] ["f2" "f3"] ["f2" "f4"] ["g1" "f3"] ["g1" "h3"] ["e2" "e3"] ["e2" "e4"] ["d2" "d3"] ["d2" "d4"] ["c2" "c3"] ["c2" "c4"] ["b2" "b3"] ["b2" "b4"] ["a2" "a3"] ["a2" "a4"] ["b1" "c3"] ["b1" "a3"]))))))

;;;          (count '(["h2" "h3"] ["h2" "h4"] ["g1" "f3"] ["g1" "h3"] ["a2" "a3"] ["a2" "a4"] ["g2" "g3"] ["g2" "g4"] ["e2" "e3"] ["e2" "e4"] ["b1" "c3"] ["b1" "a3"] ["f2" "f3"] ["f2" "f4"] ["d2" "d3"] ["d2" "d4"] ["b2" "b3"] ["b2" "b4"] ["c2" "c3"] ["c2" "c4"]))

(deftest test-all-possible-move-with-in-check1
  (testing "black turn, only one move is allowed"
    (is (= (move-xymap2move-vec (all-possible-moves-with-in-check in-check-board black []))
           '(["e7" "d6"])))))

(deftest test-all-possible-move-with-in-check2
  (testing "black's turn, shouldn't be able to move pawn on f7 because it would put itself into check"
    (is (= (filter (fn [[from to]] (= from "f7")) (all-possible-moves-with-in-check could-become-in-check-board black []))
           '()))))
(deftest test-all-possible-move-with-in-check3
  (testing "black's turn, no possibility"
    (is (= (all-possible-moves-with-in-check check-mate-board black [])
           '()))))

(deftest test-all-possible-move-with-in-check-enpassant
  (testing ""
    (is (true? (some (fn [move] (= (movemap2move move) [(pos2coord "d5") (pos2coord "c6")])) (all-possible-moves-with-in-check en-passant-check-board white [["c7" "c5"]]))))))

(deftest test-enpassant-valid
  (testing "test en-passant move validity"
    (is (is-move-valid? en-passant-check-board white [["c7" "c5"]]  ["d5" "c6"])
        true)))
(deftest test-enpassant-valid2
  (testing "test en-passant move validity"
    (is (is-move-valid? en-passant-check-board white [["d4" "d5"]["e7" "e5"]] ["d5" "e6"])
        true)))
(deftest test-none-enpassant-diagonal-valid3
  (testing "test diagonal move validity with pawn"
    (is (is-move-valid? en-passant-check-board2 white [["d4" "d5"] ["c7" "c6"]] ["d5" "c6"])
         true)))


(deftest test-check-detection
  (testing "that a check is not detected"
    (is (= (check? (initial-board) black [])
           false))))
(deftest test-check-detection2
  (testing "that a check is detected"
    (is (= (check? in-check-board black [])
           true))))
(deftest test-check-detection3
  (testing "that a check is detected"
    (is (= (check? check-mate-board black [])
           true))))

(deftest test-check-mate-detection
  (testing "check-mate not detected"
    (is (= (check-mate? (initial-board) black [])
           false))))
(deftest test-check-mate-detection
  (testing "in check but check-mate not detected"
    (is (= (check-mate? in-check-board black [])
           false))))
(deftest test-check-mate-detection
  (testing "check-mate is detected"
    (is (= (check-mate? check-mate-board black [])
           true))))


(deftest a-check-mate-game
  (testing "playing a check mate scenario where white wins"
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["d7" "d6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "f7"] ["e8" "e7"]])
         {:score [1 0]
          :history [["e2" "e4"] ["e7" "e5"]
                    ["d1" "h5"] ["d7" "d6"]
                    ["f1" "c4"] ["b8" "c6"]
                    ["h5" "f7"] ]
          :board [:r :. :b :q :k :b :n :r
            :p :p :p :. :. :Q :p :p
            :. :. :n :p :. :. :. :.
            :. :. :. :. :p :. :. :.
            :. :. :B :. :P :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :. :K :. :N :R]
           :result :check-mate}))))



(deftest an-invalid-move-from-white-game
  (testing "invalid move from white."
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["d7" "d6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "h8"] ["e8" "d8"]])
         {:score [0 1]
          :history [["e2" "e4"] ["e7" "e5"]
                  ["d1" "h5"] ["d7" "d6"]
                  ["f1" "c4"] ["b8" "c6"]
                  ["h5" "h8"] ]
          :board [:r :. :b :q :k :b :n :r
            :p :p :p :. :. :p :p :p
            :. :. :n :p :. :. :. :.
            :. :. :. :. :p :. :. :Q
            :. :. :B :. :P :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :. :K :. :N :R]
          :result :invalid-move}))))

(deftest a-subtle-invalid-move-from-white-game
  (testing "invalid move from white. queen move is not a diagonal or a horizontal"
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["d7" "d6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "f6"] ["e8" "d8"]])
         {:score [0 1]
          :history [["e2" "e4"] ["e7" "e5"]
                  ["d1" "h5"] ["d7" "d6"]
                  ["f1" "c4"] ["b8" "c6"]
                  ["h5" "f6"] ]
          :board [:r :. :b :q :k :b :n :r
            :p :p :p :. :. :p :p :p
            :. :. :n :p :. :. :. :.
            :. :. :. :. :p :. :. :Q
            :. :. :B :. :P :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :. :K :. :N :R]
          :result :invalid-move}))))

(deftest an-invalid-move-game2
  (testing "invalid move from black."
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                          ["d1" "h5"] ["d7" "d6"]
                          ["f1" "c4"] ["b8" "c6"]
                          ["h5" "g6"] ["e8" "d8"]])
         {:score [1 0]
          :history [["e2" "e4"] ["e7" "e5"]
                    ["d1" "h5"] ["d7" "d6"]
                    ["f1" "c4"] ["b8" "c6"]
                    ["h5" "g6"] ["e8" "d8"]]
          :board [:r :. :b :q :k :b :n :r
           :p :p :p :. :. :p :p :p
           :. :. :n :p :. :. :Q :.
           :. :. :. :. :p :. :. :.
           :. :. :B :. :P :. :. :.
           :. :. :. :. :. :. :. :.
           :P :P :P :P :. :P :P :P
           :R :N :B :. :K :. :N :R]
          :result :invalid-move}))))

(deftest cannot-move-into-check-case-game
  (testing "black cannot move it's pawn on f7 because it will get into check mate. However it decides to make an invalid move [f7 f6] anyway."
    (is (=
         (play-scenario  [["e2" "e4"] ["e7" "e5"]
                           ["d1" "h5"] ["f7" "f6"]
                           ["f1" "c4"] ["b8" "c6"]
                           ["h5" "f7"] ["e8" "e7"]])
         {:score [1 0]
          :history [["e2" "e4"] ["e7" "e5"]
                  ["d1" "h5"] ["f7" "f6"]]
          :board [:r :n :b :q :k :b :n :r
            :p :p :p :p :. :p :p :p
            :. :. :. :. :. :. :. :.
            :. :. :. :. :p :. :. :Q
            :. :. :. :. :P :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :. :K :B :N :R]
          :result :invalid-move}))))

(deftest en-passant-case-game
  (testing ""
    (is (=
         (play-scenario  [["e2" "e4"] ["d7" "d5"]
                          ["e4" "d5"] ["e7" "e5"]
                          ["d5" "e6"] ["d5" "e6"]])
         {:score [1 0]
          :history [["e2" "e4"] ["d7" "d5"]
                  ["e4" "d5"] ["e7" "e5"]
                  ["d5" "e6"] ["d5" "e6"]]
          :board [:r :n :b :q :k :b :n :r
            :p :p :p :. :. :p :p :p
            :. :. :. :. :P :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :Q :K :B :N :R]
          :result :invalid-move}))))

(deftest en-passant-case-game2
  (testing ""
    (is (=
         (play-scenario  [["e2" "e4"] ["d7" "d5"]
                          ["e4" "d5"] ["c7" "c5"]
                          ["d5" "c6"] ["d5" "c6"]])
         {:score [1 0]
          :history [["e2" "e4"] ["d7" "d5"]
                  ["e4" "d5"] ["c7" "c5"]
                  ["d5" "c6"] ["d5" "c6"]]
          :board [:r :n :b :q :k :b :n :r
            :p :p :. :. :p :p :p :p
            :. :. :P :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :Q :K :B :N :R]
          :result :invalid-move}))))

(deftest en-passant-case-game3
  (testing ""
    (is (=
         (play-scenario  [["e2" "e4"] ["f7" "f5"]
                          ["g2" "g4"] ["f5" "f4"]
                          ["b1" "a3"] ["f4" "e3"]])
         {:score [1 0]
          :history [["e2" "e4"] ["f7" "f5"]
                  ["g2" "g4"] ["f5" "f4"]
                  ["b1" "a3"] ["f4" "e3"]]
          :board [:r :n :b :q :k :b :n :r
            :p :p :p :p :p :. :p :p
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :P :p :P :.
            :N :. :. :. :. :. :. :.
            :P :P :P :P :. :P :. :P
            :R :. :B :Q :K :B :N :R]

          :result :invalid-move}))))

(deftest en-passant-case-game4
  (testing ""
    (is (=
         (play-scenario  [["e2" "e4"] ["f7" "f5"]
                          ["b1" "a3"] ["f5" "f4"]
                          ["g2" "g4"] ["f4" "g3"]
                          nil])
         {:score [0 1]
          :history [["e2" "e4"] ["f7" "f5"]
                  ["b1" "a3"] ["f5" "f4"]
                  ["g2" "g4"] ["f4" "g3"]
                  nil]
          :board [:r :n :b :q :k :b :n :r
            :p :p :p :p :p :. :p :p
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :P :. :. :.
            :N :. :. :. :. :. :p :.
            :P :P :P :P :. :P :. :P
            :R :. :B :Q :K :B :N :R]

          :result :invalid-move}))))

(deftest en-passant-case-game5
  (testing ""
    (is (=
         (play-scenario  [["g2" "g4"] ["f7" "f5"]
                          ["b1" "a3"] ["f5" "f4"]
                          ["e2" "e4"] ["f4" "g3"]
                          nil])
         {:score [1 0]
          :history [["g2" "g4"] ["f7" "f5"]
                  ["b1" "a3"] ["f5" "f4"]
                  ["e2" "e4"] ["f4" "g3"]
                  ]
          :board [:r :n :b :q :k :b :n :r
            :p :p :p :p :p :. :p :p
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :P :p :P :.
            :N :. :. :. :. :. :. :.
            :P :P :P :P :. :P :. :P
            :R :. :B :Q :K :B :N :R]

          :result :invalid-move}))))

(deftest en-passant-case-game6
  (testing ""
    (is (=
         (play-scenario  [["g2" "g4"] ["f7" "f5"]
                          ["b1" "a3"] ["f5" "f4"]
                          ["e2" "e4"] ["f4" "e3"]
                          nil])
         {:score [0 1]
          :history [["g2" "g4"] ["f7" "f5"]
                  ["b1" "a3"] ["f5" "f4"]
                  ["e2" "e4"] ["f4" "e3"]
                  nil]
          :board [:r :n :b :q :k :b :n :r
            :p :p :p :p :p :. :p :p
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :P :.
            :N :. :. :. :p :. :. :.
            :P :P :P :P :. :P :. :P
            :R :. :B :Q :K :B :N :R]

          :result :invalid-move}))))


(defn invalid-move-f [_]
  nil)
(defn garbage-f [_]
  -1)

(def ex (Exception. "this is an exception"))

(defn exception-f [_]
  (throw ex))



(deftest function-raises-exception
  (testing ""
    (is (= (dissoc (play-game {:board (initial-board) :f1 exception-f :f2 exception-f}) :stacktrace)
           {:score [0 1]
            :history [:exception]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :exception "java.lang.Exception: this is an exception"
            :result :caught-exception}))))
(deftest function-garbage-move
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 garbage-f :f2 garbage-f})
           {:score [0 1]
            :history [-1]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))



(deftest function-garbage-move2
  (testing ""
    (is (= (play-game {:board (initial-board) :f1  (fn [_] {:move ["e9" "e4"]}) :f2 garbage-f})
           {:score [0 1]
            :history [["e9" "e4"]]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

;; (play-game-seq game-step (merge {:board (initial-board) :f1  (fn [_] ["e8" "A4"]) :f2 garbage-f} {:board (initial-board) :history []}))

(deftest function-garbage-move3
  (testing "upper case move don't work"
    (is (= (play-game {:board (initial-board) :f1  (fn [_] ["e8" "A4"]) :f2 garbage-f})
           {:score [0 1]
            :history [["e8" "A4"]]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest function-garbage-move4
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] ["ee8" "a4"]) :f2 garbage-f})
           {:score [0 1]
            :history [["ee8" "a4"]]
            :board (initial-board)
            :result :invalid-move}))))

(deftest function-garbage-move5
  (testing ""
    (is (= (play-game {:board (initial-board) :f1  (fn [_] ["e0" "a4"]) :f2 garbage-f})
           {:score [0 1]
            :history [["e0" "a4"]]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest function-f1-vs-f2-check-mate
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 f1 :f2 f2})
           {:score [1 0]
          :history [["e2" "e4"] ["e7" "e5"]
                    ["d1" "h5"] ["d7" "d6"]
                    ["f1" "c4"] ["b8" "c6"]
                    ["h5" "f7"] ]
          :board [:r :. :b :q :k :b :n :r
            :p :p :p :. :. :Q :p :p
            :. :. :n :p :. :. :. :.
            :. :. :. :. :p :. :. :.
            :. :. :B :. :P :. :. :.
            :. :. :. :. :. :. :. :.
            :P :P :P :P :. :P :P :P
            :R :N :B :. :K :. :N :R]
           :result :check-mate}))))

(def weird-obj (java.lang.Thread.))

(deftest function-garbage-move6
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] {:move [weird-obj ""] :state nil}) :f2 garbage-f})
           {:score [0 1]
            :history [[weird-obj ""]]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest function-garbage-move6
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] [[weird-obj ""] nil]) :f2 garbage-f})
           {:score [0 1]
            :history [[[weird-obj ""] nil]]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest function-garbage-move7
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] [[-1 0.9] [true false]]) :f2 garbage-f})
           {:score [0 1]
            :history [[[-1 0.9] [true false]]]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))


(deftest function-nil-move
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 invalid-move-f :f2 invalid-move-f})
           {:score [0 1]
            :history [nil]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest function-nil-move2
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] ["e2" "e4"]) :f2 invalid-move-f})
           {:score [1 0]
            :history [["e2" "e4"] nil]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :P :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :. :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest function-nil-move3
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] ["e2" "e4"]) :f2 invalid-move-f})
           {:score [1 0]
            :history [["e2" "e4"] nil]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :P :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :. :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest keyword_move
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] [:e2 :e4]) :f2 invalid-move-f})
           {:score [1 0]
            :history [["e2" "e4"] nil]
            :board [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :P :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :. :P :P :P
              :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest keyword_move_bad_format
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (sb '(fn [_] [:e9 :e4])) :f2 invalid-move-f})
           {:score [0 1]
            :history [["e9" "e4"]]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest keyword_move_bad_format
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] [:w7]) :f2 invalid-move-f})
           {:score [0 1]
            :history [[:w7]]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

(deftest en-passant-case-game3
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (fn [_] [:w7]) :f2 invalid-move-f})
           {:score [0 1]
            :history [[:w7]]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :result :invalid-move}))))

;; (require '[clojure.data :refer [diff]])
;; (= (play-scenario  [[:a2 :a3] [:f7 :f5] [:c2 :c4] [:e7 :e5] [:d1 :a4] [:d8 :f6] [:d2 :d3] [:f6 :h4] [:g2 :g4] [:b8 :c6] [:b2 :b4] [:g7 :g6] [:c1 :h6] [:f8 :h6] [:e2 :e3] [:h4 :h3] [:g4 :f5] [:h6 :g5] [:g1 :f3] [:g8 :e7] [:e1 :d2] [:h3 :h5] [:a4 :c2] [:e7 :d5] [:c4 :d5] [:g5 :f6] [:c2 :a4] [:e5 :e4] [:f3 :g5] [:f6 :b2] [:a4 :a5] [:b2 :c1] [:d2 :c1] [:h5 :e2] [:a5 :c7] [:e2 :g4] [:c7 :c6] [:g4 :h4] [:h1 :g1] [:h4 :f2] [:d3 :d4] [:f2 :g2] [:b4 :b5] [:b7 :c6] [:g5 :h7] [:e8 :e7] [:a3 :a4] [:g2 :g4] [:b1 :d2] [:c8 :a6] [:h2 :h4] [:a6 :b7] [:g1 :g4] [:e7 :d6] [:g4 :g2] [:a8 :d8] [:f1 :e2] [:c6 :d5] nil])
;;                    {:score [0 1], :history [["a2" "a3"] ["f7" "f5"] ["c2" "c4"] ["e7" "e5"] ["d1" "a4"] ["d8" "f6"] ["d2" "d3"] ["f6" "h4"] ["g2" "g4"] ["b8" "c6"] ["b2" "b4"] ["g7" "g6"] ["c1" "h6"] ["f8" "h6"] ["e2" "e3"] ["h4" "h3"] ["g4" "f5"] ["h6" "g5"] ["g1" "f3"] ["g8" "e7"] ["e1" "d2"] ["h3" "h5"] ["a4" "c2"] ["e7" "d5"] ["c4" "d5"] ["g5" "f6"] ["c2" "a4"] ["e5" "e4"] ["f3" "g5"] ["f6" "b2"] ["a4" "a5"] ["b2" "c1"] ["d2" "c1"] ["h5" "e2"] ["a5" "c7"] ["e2" "g4"] ["c7" "c6"] ["g4" "h4"] ["h1" "g1"] ["h4" "f2"] ["d3" "d4"] ["f2" "g2"] ["b4" "b5"] ["b7" "c6"] ["g5" "h7"] ["e8" "e7"] ["a3" "a4"] ["g2" "g4"] ["b1" "d2"] ["c8" "a6"] ["h2" "h4"] ["a6" "b7"] ["g1" "g4"] ["e7" "d6"] ["g4" "g2"] ["a8" "d8"] ["f1" "e2"] ["c6" "d5"] nil],
;;           :board
;;           [:. :. :. :r :. :. :. :r
;;            :p :b :. :p :. :. :. :N
;;            :. :. :. :k :. :. :p :.
;;            :. :P :. :p :. :P :. :.
;;            :P :. :. :P :p :. :. :P
;;            :. :. :. :. :P :. :. :.
;;            :. :. :. :N :B :. :R :.
;;            :R :. :K :. :. :. :. :.], :result :invalid-move})

(deftest dont-kill-the-king
  (testing ""
    (is (=
         (play-scenario  [[:a2 :a3] [:f7 :f5] [:c2 :c4] [:e7 :e5] [:d1 :a4] [:d8 :f6] [:d2 :d3] [:f6 :h4] [:g2 :g4] [:b8 :c6] [:b2 :b4] [:g7 :g6] [:c1 :h6] [:f8 :h6] [:e2 :e3] [:h4 :h3] [:g4 :f5] [:h6 :g5] [:g1 :f3] [:g8 :e7] [:e1 :d2] [:h3 :h5] [:a4 :c2] [:e7 :d5] [:c4 :d5] [:g5 :f6] [:c2 :a4] [:e5 :e4] [:f3 :g5] [:f6 :b2] [:a4 :a5] [:b2 :c1] [:d2 :c1] [:h5 :e2] [:a5 :c7] [:e2 :g4] [:c7 :c6] [:g4 :h4] [:h1 :g1] [:h4 :f2] [:d3 :d4] [:f2 :g2] [:b4 :b5] [:b7 :c6] [:g5 :h7] [:e8 :e7] [:a3 :a4] [:g2 :g4] [:b1 :d2] [:c8 :a6] [:h2 :h4] [:a6 :b7] [:g1 :g4] [:e7 :d6] [:g4 :g2] [:a8 :d8] [:f1 :e2] [:c6 :d5] nil])
         {:score [0 1], :history [["a2" "a3"] ["f7" "f5"] ["c2" "c4"] ["e7" "e5"] ["d1" "a4"] ["d8" "f6"] ["d2" "d3"] ["f6" "h4"] ["g2" "g4"] ["b8" "c6"] ["b2" "b4"] ["g7" "g6"] ["c1" "h6"] ["f8" "h6"] ["e2" "e3"] ["h4" "h3"] ["g4" "f5"] ["h6" "g5"] ["g1" "f3"] ["g8" "e7"] ["e1" "d2"] ["h3" "h5"] ["a4" "c2"] ["e7" "d5"] ["c4" "d5"] ["g5" "f6"] ["c2" "a4"] ["e5" "e4"] ["f3" "g5"] ["f6" "b2"] ["a4" "a5"] ["b2" "c1"] ["d2" "c1"] ["h5" "e2"] ["a5" "c7"] ["e2" "g4"] ["c7" "c6"] ["g4" "h4"] ["h1" "g1"] ["h4" "f2"] ["d3" "d4"] ["f2" "g2"] ["b4" "b5"] ["b7" "c6"] ["g5" "h7"] ["e8" "e7"] ["a3" "a4"] ["g2" "g4"] ["b1" "d2"] ["c8" "a6"] ["h2" "h4"] ["a6" "b7"] ["g1" "g4"] ["e7" "d6"] ["g4" "g2"] ["a8" "d8"] ["f1" "e2"] ["c6" "d5"] nil],
          :board
          [:. :. :. :r :. :. :. :r
           :p :b :. :p :. :. :. :N
           :. :. :. :k :. :. :p :.
           :. :P :. :p :. :P :. :.
           :P :. :. :P :p :. :. :P
           :. :. :. :. :P :. :. :.
           :. :. :. :N :B :. :R :.
           :R :. :K :. :. :. :. :.], :result :invalid-move}))))

(deftest dont-kill-the-king2
  (testing ""
    (is (= (time (play-game-rec {:board [:. :. :. :r :. :. :. :r
                                    :p :b :. :p :. :. :. :N
                                    :. :. :p :k :. :. :p :.
                                    :. :P :. :P :. :P :. :.
                                    :P :. :. :P :p :. :. :P
                                    :. :. :. :. :P :. :. :.
                                    :. :. :. :N :B :. :R :.
                                    :R :. :K :. :. :. :. :.] :f1 (fn [_] nil) :f2 (fn [_] [:c6 :d5]) :is-player1-turn black :history [ ["a2" "a3"] ["f7" "f5"] ["c2" "c4"] ["e7" "e5"] ["d1" "a4"] ["d8" "f6"] ["d2" "d3"] ["f6" "h4"] ["g2" "g4"] ["b8" "c6"] ["b2" "b4"] ["g7" "g6"] ["c1" "h6"] ["f8" "h6"] ["e2" "e3"] ["h4" "h3"] ["g4" "f5"] ["h6" "g5"] ["g1" "f3"] ["g8" "e7"] ["e1" "d2"] ["h3" "h5"] ["a4" "c2"] ["e7" "d5"] ["c4" "d5"] ["g5" "f6"] ["c2" "a4"] ["e5" "e4"] ["f3" "g5"] ["f6" "b2"] ["a4" "a5"] ["b2" "c1"] ["d2" "c1"] ["h5" "e2"] ["a5" "c7"] ["e2" "g4"] ["c7" "c6"] ["g4" "h4"] ["h1" "g1"] ["h4" "f2"] ["d3" "d4"] ["f2" "g2"] ["b4" "b5"] ["b7" "c6"] ["g5" "h7"] ["e8" "e7"] ["a3" "a4"] ["g2" "g4"] ["b1" "d2"] ["c8" "a6"] ["h2" "h4"] ["a6" "b7"] ["g1" "g4"] ["e7" "d6"] ["g4" "g2"] ["a8" "d8"] ["f1" "e2"] ]}))
           {:score [0 1], :history [["a2" "a3"] ["f7" "f5"] ["c2" "c4"] ["e7" "e5"] ["d1" "a4"] ["d8" "f6"] ["d2" "d3"] ["f6" "h4"] ["g2" "g4"] ["b8" "c6"] ["b2" "b4"] ["g7" "g6"] ["c1" "h6"] ["f8" "h6"] ["e2" "e3"] ["h4" "h3"] ["g4" "f5"] ["h6" "g5"] ["g1" "f3"] ["g8" "e7"] ["e1" "d2"] ["h3" "h5"] ["a4" "c2"] ["e7" "d5"] ["c4" "d5"] ["g5" "f6"] ["c2" "a4"] ["e5" "e4"] ["f3" "g5"] ["f6" "b2"] ["a4" "a5"] ["b2" "c1"] ["d2" "c1"] ["h5" "e2"] ["a5" "c7"] ["e2" "g4"] ["c7" "c6"] ["g4" "h4"] ["h1" "g1"] ["h4" "f2"] ["d3" "d4"] ["f2" "g2"] ["b4" "b5"] ["b7" "c6"] ["g5" "h7"] ["e8" "e7"] ["a3" "a4"] ["g2" "g4"] ["b1" "d2"] ["c8" "a6"] ["h2" "h4"] ["a6" "b7"] ["g1" "g4"] ["e7" "d6"] ["g4" "g2"] ["a8" "d8"] ["f1" "e2"] ["c6" "d5"] nil],
            :board [:. :. :. :r :. :. :. :r
                    :p :b :. :p :. :. :. :N
                    :. :. :. :k :. :. :p :.
                    :. :P :. :p :. :P :. :.
                    :P :. :. :P :p :. :. :P
                    :. :. :. :. :P :. :. :.
                    :. :. :. :N :B :. :R :.
                    :R :. :K :. :. :. :. :.],
            :result :invalid-move}))))


(def f1-form-test
  '(fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history s :state}]
     (let [move-seq (if (nil? s)
                      (list ["e2" "e4"] ["d1" "h5"] ["f1" "c4"] ["h5" "f7"])
                      s)]
       {:move (first move-seq) :state (into [] (next move-seq))}))
)

(def f2-form-test
  '(fn [{board :board am-i-white? :is-player1-turn ic :in-check? h :history option-state :state}]
     (let [b board
           move-seq (if (nil? option-state)
                      (list ["e7" "e5"] ["d7" "d6"] ["b8" "c6"] ["e8" "e7"])
                      option-state)]
       {:move (first move-seq) :state (into [] (next move-seq))}))
)


(deftest sand-boxed-f1-vs-f2
  (testing ""
    (is (= (play-game {:board (initial-board) :f1 (sb f1-form-test) :f2 (sb f2-form-test)})
           {:score [1 0], :history [["e2" "e4"] ["e7" "e5"]
                                    ["d1" "h5"] ["d7" "d6"]
                                    ["f1" "c4"] ["b8" "c6"]
                                    ["h5" "f7"]],
            :board [:r :. :b :q :k :b :n :r
                    :p :p :p :. :. :Q :p :p
                    :. :. :n :p :. :. :. :.
                    :. :. :. :. :p :. :. :.
                    :. :. :B :. :P :. :. :.
                    :. :. :. :. :. :. :. :.
                    :P :P :P :P :. :P :P :P
                    :R :N :B :. :K :. :N :R], :result :check-mate}))))

(deftest security-infinite-loop
  (testing ""
    (is (= (dissoc (play-game {:board (initial-board) :f1 (sb '(fn [_] (loop [] (recur))) 5000) :f2 invalid-move-f}) :stacktrace)
           {:score [0 1]
            :history [:exception]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :exception "java.util.concurrent.TimeoutException: Execution timed out."
            :result :too-slow-to-move}))))

(deftest security-spawn-thread
  (testing ""
    (is (= (dissoc (play-game {:board (initial-board) :f1 (sb '(fn [_] (Thread.))) :f2 invalid-move-f}) :stacktrace)
           {:score [0 1]
            :history [:exception]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :exception "java.lang.SecurityException: You tripped the alarm! class java.lang.Thread is bad!"
            :result :security-exception}))))

(deftest wrong-arrity-function
  (testing ""
    (is (= (dissoc (play-game {:board (initial-board) :f1 (sb '(fn [_ _] [:e5 :e6])) :f2 invalid-move-f}) :stacktrace :exception)
           {:score [0 1]
            :history [:exception]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :result :caught-exception}))))

(deftest not-compiling-function
  (testing ""
    (is (= (dissoc (play-game {:board (initial-board) :f1 (sb '((fn) [_] [:e5 :e6])) :f2 invalid-move-f :id1 "daredevil" :id2 "wonderboy"}) :stacktrace)
           {:score [0 1]
            :history [:exception]
            :board [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :P :P :P :P
             :R :N :B :Q :K :B :N :R]
            :exception "java.lang.IllegalArgumentException: Parameter declaration missing"
            :result :caught-exception}))))


(deftest replaying-game
  (testing ""
    (is (= (board-seq [["e2" "e4"] ["e7" "e5"] ["d1" "h5"] ["d7" "d6"] ["f1" "c4"] ["b8" "c6"] ["h5" "f7"] ["e8" "e7"]] )
           (vector
            [:r :n :b :q :k :b :n :r
              :p :p :p :p :p :p :p :p
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :. :. :. :. :. :. :. :.
              :P :P :P :P :P :P :P :P
              :R :N :B :Q :K :B :N :R]

            [:r :n :b :q :k :b :n :r
             :p :p :p :p :p :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :. :. :. :.
             :. :. :. :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :Q :K :B :N :R]

            [:r :n :b :q :k :b :n :r
             :p :p :p :p :. :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :p :. :. :.
             :. :. :. :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :Q :K :B :N :R]

            [:r :n :b :q :k :b :n :r
             :p :p :p :p :. :p :p :p
             :. :. :. :. :. :. :. :.
             :. :. :. :. :p :. :. :Q
             :. :. :. :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :. :K :B :N :R]

            [:r :n :b :q :k :b :n :r
             :p :p :p :. :. :p :p :p
             :. :. :. :p :. :. :. :.
             :. :. :. :. :p :. :. :Q
             :. :. :. :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :. :K :B :N :R]

            [:r :n :b :q :k :b :n :r
             :p :p :p :. :. :p :p :p
             :. :. :. :p :. :. :. :.
             :. :. :. :. :p :. :. :Q
             :. :. :B :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :. :K :. :N :R]

            [:r :. :b :q :k :b :n :r
             :p :p :p :. :. :p :p :p
             :. :. :n :p :. :. :. :.
             :. :. :. :. :p :. :. :Q
             :. :. :B :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :. :K :. :N :R]

            [:r :. :b :q :k :b :n :r
             :p :p :p :. :. :Q :p :p
             :. :. :n :p :. :. :. :.
             :. :. :. :. :p :. :. :.
             :. :. :B :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :. :K :. :N :R]

            ;; this move is not valid but because it's a replay it does not matter, chess engine rules are not enforced
            [:r :. :b :q :. :b :n :r
             :p :p :p :. :k :Q :p :p
             :. :. :n :p :. :. :. :.
             :. :. :. :. :p :. :. :.
             :. :. :B :. :P :. :. :.
             :. :. :. :. :. :. :. :.
             :P :P :P :P :. :P :P :P
             :R :N :B :. :K :. :N :R])
           ))))
