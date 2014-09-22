(ns zone.lambda.game.board-test
  (:require [zone.lambda.game.board :as board :refer :all]
            [clojure.test :refer :all]))

(def column-nb 9)
(def raw-nb 10)
;; (def column-nb 8)
;; (def raw-nb 8)
(defn initial-board []
  [:r :n :b :q :k :b :n :r :.
   :p :p :p :p :p :p :p :p :.
   :. :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :. :.
   :P :P :P :P :P :P :P :P :.
   :R :N :B :Q :K :B :N :R :.
   :. :. :. :. :. :. :. :. :.
   :. :. :. :. :. :. :. :. :.])

;; (defn initial-board []
;;   [:r :n :b :q :k :b :n :r
;;    :p :p :p :p :p :p :p :p
;;    :. :. :. :. :. :. :. :.
;;    :. :. :. :. :. :. :. :.
;;    :. :. :. :. :. :. :. :.
;;    :. :. :. :. :. :. :. :.
;;    :P :P :P :P :P :P :P :P
;;    :R :N :B :Q :K :B :N :R])


(def nothing-between-test (partial nothing-between column-nb raw-nb (initial-board)))

(deftest test-nothing-betwen
  (testing ""
    (is (= (map #(apply nothing-between-test %)
                [[[0 0] [7 7]]
                 [[1 1] [6 6]]
                 [[2 0] [6 0]]
                 [[0 1] [0 6]]
                 [[1 1] [7 7]]
                 [[0 0] [8 8]]
                 [[0 0] [8 0]]
                 [[0 8] [8 8]]])
           '(false true false true false false false true)))))

(deftest test-pos-betwen
  (testing ""
    (is (= (map #(apply pos-between %)
                [[[0 0] [7 7]]
                 [[1 1] [6 6]]
                 [[2 0] [6 0]]
                 [[0 1] [0 6]]
                 [[7 7] [0 0]]
                 [[0 0] [8 8]]
                 [[0 0] [8 0]]
                 [[0 8] [9 8]]])
           '(([1 1] [2 2] [3 3] [4 4] [5 5] [6 6])
             ([2 2] [3 3] [4 4] [5 5])
             ([3 0] [4 0] [5 0])
             ([0 2] [0 3] [0 4] [0 5])
             ([1 1] [2 2] [3 3] [4 4] [5 5] [6 6])
             ([1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7])
             ([1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0])
             ([1 8] [2 8] [3 8] [4 8] [5 8] [6 8] [7 8] [8 8]))))))

(deftest test-1d-2d-conversion
  (testing ""
    (is (= (map #(c2dto1d column-nb %) (map #(c1dto2d column-nb %) (range (* raw-nb column-nb))))
           (range 90)))))

(deftest test-1d-2d-conversion2
  (testing ""
    (is (= (map #(index-xy column-nb (get % 0) (get % 1)) (map #(c1dto2d column-nb %) (range (* raw-nb column-nb))))
           (range 90)))))

(deftest test-1d-2d-conversion3
  (testing ""
    (is (= (map #(c1dto2d column-nb %) (range (* raw-nb column-nb)))
           '( [0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0] [8 0]
              [0 1] [1 1] [2 1] [3 1] [4 1] [5 1] [6 1] [7 1] [8 1]
              [0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2] [8 2]
              [0 3] [1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [7 3] [8 3]
              [0 4] [1 4] [2 4] [3 4] [4 4] [5 4] [6 4] [7 4] [8 4]
              [0 5] [1 5] [2 5] [3 5] [4 5] [5 5] [6 5] [7 5] [8 5]
              [0 6] [1 6] [2 6] [3 6] [4 6] [5 6] [6 6] [7 6] [8 6]
              [0 7] [1 7] [2 7] [3 7] [4 7] [5 7] [6 7] [7 7] [8 7]
              [0 8] [1 8] [2 8] [3 8] [4 8] [5 8] [6 8] [7 8] [8 8]
              [0 9] [1 9] [2 9] [3 9] [4 9] [5 9] [6 9] [7 9] [8 9])))))

(deftest test-board2xy-map-piece
  (testing ""
    (is (= (board2xy-map-piece column-nb raw-nb (initial-board))
           {[0 0] :r, [1 0] :n, [2 0] :b, [3 0] :q, [4 0] :k, [5 0] :b, [6 0] :n, [7 0] :r,
            [0 1] :p, [1 1] :p, [2 1] :p, [3 1] :p, [4 1] :p, [5 1] :p, [6 1] :p, [7 1] :p,

            [0 6] :P, [1 6] :P, [2 6] :P, [3 6] :P, [4 6] :P, [5 6] :P, [6 6] :P, [7 6] :P,
            [0 7] :R, [1 7] :N, [2 7] :B, [3 7] :Q, [4 7] :K, [5 7] :B, [6 7] :N, [7 7] :R }))))

(deftest test-file-component
  (testing ""
    (is (= [(file-component \:)
            (file-component \a)
            (file-component \b)
            (file-component \c)
            (file-component \d)
            (file-component \e)
            (file-component \E)
            (file-component \f)
            (file-component \g)
            (file-component \h)]
           [-39 0 1 2 3 4 -28 5 6 7]))))

(def rank-component-test (partial rank-component column-nb))

(deftest test-rank-component
  (testing ""
    (is (= [(rank-component-test \0)
            (rank-component-test \1)
            (rank-component-test \2)
            (rank-component-test \3)
            (rank-component-test \4)
            (rank-component-test \f)
            (rank-component-test \7)
            (rank-component-test \8)
            (rank-component-test \9)]
           [81 72 63 54 45 -405 18 9 0]))))

(deftest test-rank-component2
  (testing ""
    (is (= [(rank-component 8 \0)
            (rank-component 8 \1)
            (rank-component 8 \2)
            (rank-component 8 \3)
            (rank-component 8 \4)
            (rank-component 8 \f)
            (rank-component 8 \7)
            (rank-component 8 \8)
            (rank-component 8 \9)]
           [64 56 48 40 32 -368 8 0 -8]))))

(deftest test-rank2coord
  (testing ""
    (is (= (map #(rank2coord raw-nb %)
                [\: \9 \8 \7 \6 \5 \4 \3 \2 \1])

           '(0 1 2 3 4 5 6 7 8 9)
           ))))

(deftest test-rank2coord-chess
  (testing ""
    (is (= (map #(rank2coord 8 %)
                [\8 \7 \6 \5 \4 \3 \2 \1])

           '(0 1 2 3 4 5 6 7)
           ))))


(deftest test-coord2pos1
  (testing ""
    (is (= (map #(coord2pos column-nb raw-nb %) (for [y (range 0 raw-nb)
                                                  x (range 0 column-nb)
                                          ]
                                                  [x y]))
           ["a:" "b:" "c:" "d:" "e:" "f:" "g:" "h:" "i:"
            "a9" "b9" "c9" "d9" "e9" "f9" "g9" "h9" "i9"
            "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8" "i8"
            "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7" "i7"
            "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6" "i6"
            "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5" "i5"
            "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4" "i4"
            "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3" "i3"
            "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2"
            "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1"]
           ))))

(deftest test-coord2pos-chess
  (testing ""
    (is (= (map #(coord2pos 8 8 %) (for [y (range 0 8)
                                         x (range 0 8)
                                          ]
                                     [x y]))
           [
            "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"
            "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
            "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6"
            "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5"
            "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4"
            "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3"
            "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
            "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1"]
           ))))

(pos2coord raw-nb "i1")

(deftest test-pos2coord1
  (testing ""
    (is (= (map #(pos2coord raw-nb %)
                ["a:" "b:" "c:" "d:" "e:" "f:" "g:" "h:" "i:"
            "a9" "b9" "c9" "d9" "e9" "f9" "g9" "h9" "i9"
            "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8" "i8"
            "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7" "i7"
            "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6" "i6"
            "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5" "i5"
            "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4" "i4"
            "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3" "i3"
            "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2"
            "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1"])

           (for [y (range 0 raw-nb)
                 x (range 0 column-nb)
                 ]
             [x y])
           ))))

(deftest test-pos2coord-chess
  (testing ""
    (is (= (map #(pos2coord 8 %)           [
            "a8" "b8" "c8" "d8" "e8" "f8" "g8" "h8"
            "a7" "b7" "c7" "d7" "e7" "f7" "g7" "h7"
            "a6" "b6" "c6" "d6" "e6" "f6" "g6" "h6"
            "a5" "b5" "c5" "d5" "e5" "f5" "g5" "h5"
            "a4" "b4" "c4" "d4" "e4" "f4" "g4" "h4"
            "a3" "b3" "c3" "d3" "e3" "f3" "g3" "h3"
            "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
            "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1"])

           (for [y (range 0 8)
                 x (range 0 8)
                 ]
             [x y])
           ))))

(def collid?-test (partial collid? column-nb raw-nb (initial-board)))

;;(lookup-xy column-nb raw-nb (initial-board) [0 0])
;;(coord2pos column-nb raw-nb [0 0])
;;(board/coord2rank raw-nb 0)

(deftest test-collid
  (testing ""
    (is (= [(collid?-test [1 5])
            (collid?-test [0 0])
            (collid?-test [2 2])
            (collid?-test [3 3])
            (collid?-test [4 4])
            (collid?-test [4 0])
            (collid?-test [6 4])
            (collid?-test [4 6])
            (collid?-test [7 7])
            (collid?-test [7 8])
            (collid?-test [8 7])]
           [false true false false false true false true true false false]
           ))))

(def collid-self?-test (partial collid-self? column-nb raw-nb (initial-board)))
;;(collid-self?-test true [9 8])
;;(coord2pos column-nb [9 8])

(deftest test-collid-self
  (testing ""
    (is (= (map #(collid-self?-test true %) (for [y (range 0 raw-nb)
                                                  x (range 0 column-nb)
                                          ]
                                      [x y]))
           [false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            true  true  true  true  true  true  true  true  false
            true  true  true  true  true  true  true  true  false
            false false false false false false false false false
            false false false false false false false false false]

           ))))

(deftest test-collid-self2
  (testing ""
    (is (= (map #(collid-self?-test false %) (for [y (range 0 raw-nb)
                                                  x (range 0 column-nb)
                                          ]
                                      [x y]))
           [true  true  true  true  true  true  true  true  false
            true  true  true  true  true  true  true  true  false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false]

           ))))

(deftest test-collid-opposite-white
  (testing ""
    (is (= (map #(collid-oposite? column-nb raw-nb (initial-board) true %) (for [y (range 0 raw-nb)
                                                  x (range 0 column-nb)
                                          ]
                                      [x y]))
           [true  true  true  true  true  true  true  true  false
            true  true  true  true  true  true  true  true  false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false]

           ))))

(deftest test-collid-opposite-black
  (testing ""
    (is (= (map #(collid-oposite? column-nb raw-nb (initial-board) false %) (for [y (range 0 raw-nb)
                                                  x (range 0 column-nb)
                                          ]
                                      [x y]))
           [false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            false false false false false false false false false
            true  true  true  true  true  true  true  true  false
            true  true  true  true  true  true  true  true  false
            false false false false false false false false false
            false false false false false false false false false]

           ))))

(deftest test-pos-xy-within-board
  (testing ""
    (is (= (map #(pos-xy-within-board? column-nb raw-nb %)
                (for [y (range -1 (inc raw-nb))
                      x (range -1 (inc column-nb))
                      ]
                  [x y]))
           [false false false false false false false false false false false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false true  true  true  true  true  true  true  true  true  false
            false false false false false false false false false false false
            ]
           ))))


(def lookup-xy-test (partial board/lookup-xy column-nb raw-nb (initial-board)))



;;(lookup-xy-test [0 0])

(deftest test-lookup-xy
  (testing ""
    (is (= (map #(lookup-xy-test %) (for [y (range 0 raw-nb)
                                          x (range 0 column-nb)
                                          ]
                                      [x y]))
           [:r :n :b :q :k :b :n :r :.
            :p :p :p :p :p :p :p :p :.
            :. :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :. :.
            :P :P :P :P :P :P :P :P :.
            :R :N :B :Q :K :B :N :R :.
            :. :. :. :. :. :. :. :. :.
            :. :. :. :. :. :. :. :. :.]

           ))))



(comment
;; (file-component \b)
;;(rank-component column-nb \1)
;; (lookup (initial-board) "e5")
;;=> :R
  ;;(lookup-xy (initial-board) [0 7])

  ;;(index :a :1)

;;(index-xy 7 7)



;(coord2rank 5)

                                        ;(rank-coord \1)

  ;(file-coord :a)

;;(file-component :a)

;; ((fn [pieces-list]
;;    (into {} (filter #(not= :. (second %)) (map #(vector (c1dto2d %1) %2 ) (range (* raw-nb column-nb)) pieces-list)))) (initial-board))

;;(collid? column-nb (initial-board) [1 6])

;;(pos-within-board? "e8")
;;(pos-within-board? "e9")
;;(pos-within-board? "q8")


)
