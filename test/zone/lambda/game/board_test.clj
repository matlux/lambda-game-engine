(ns zone.lambda.game.board-test
  (:require [zone.lambda.game.board :refer :all]
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


(def nothing-between-test (partial nothing-between column-nb (initial-board)))

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
                 [[0 8] [8 8]]])
           '(([1 1] [2 2] [3 3] [4 4] [5 5] [6 6])
             ([2 2] [3 3] [4 4] [5 5])
             ([3 0] [4 0] [5 0])
             ([0 2] [0 3] [0 4] [0 5])
             ([1 1] [2 2] [3 3] [4 4] [5 5] [6 6])
             ([1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7])
             ([1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [7 0])
             ([1 8] [2 8] [3 8] [4 8] [5 8] [6 8] [7 8]))))))

(deftest test-1d-2d-conversion
  (testing ""
    (is (= (map #(c2dto1d column-nb %) (map #(c1dto2d column-nb %) (range (* raw-nb column-nb))))
           (range 90)))))

(deftest test-1d-2d-conversion2
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

(deftest test-file-component
  (testing ""
    (is (= [(file-component \a)
            (file-component \b)
            (file-component \c)
            (file-component \d)
            (file-component \E)
            (file-component \f)
            (file-component \g)]
           [0 1 2 3 -28 5 6]))))

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

(def collid?-test (partial collid? column-nb (initial-board)))

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
