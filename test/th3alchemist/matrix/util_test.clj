(ns th3alchemist.matrix.util-test
  (:require [clojure.test :refer :all]
            [th3alchemist.matrix.util :refer :all]))
(alias 'util 'th3alchemist.matrix.util)


(def M (with-meta [98 4 2 3 7 0]
                           {:row_cnt 3 :col_cnt 2 :col_names [:A :B :C] :row_names [:A :B]}))
(def N (with-meta [0 5 23 19 69 13]
                           {:row_cnt 2 :col_cnt 3 :col_names [:a :b] :row_names [:a :b :c]}))
(def O (with-meta [1 8 7 16 32 9 42 20 17]
                           {:row_cnt 3 :col_cnt 3 :col_names [:X :Y :Z] :row_names [:x :y :z]}))
(def singleRow (with-meta [29 84 -20 37]
                           {:row_cnt 1 :col_cnt 4 :col_names [:age] :row_names [:shayla :zamirh :codex :josier]}))
(def singleCol (with-meta [8.406 1.553 0.622 0.447]
                           {:row_cnt 4 :col_cnt 1 :col_names [:nyc :phl :stl :atl] :row_names [:population]}))
(def misMatch (with-meta [8.406 1.553 0.622 0.447]
                           {:row_cnt 4 :col_cnt 4 :col_names [:nyc :phl :stl :atl] :row_names [:population]}))
(def oddSquareMatrix (with-meta [1 8 7 16 32 9 42 20 17]
                           {:row_cnt 3 :col_cnt 3 :col_names [:X :Y :Z] :row_names [:x :y :z]}))
(def evenSquareMatrix (with-meta [14 -8 7 16 32 -9 42 20 17 -32 18 90 0 87 2 54]
                           {:row_cnt 4 :col_cnt 4 :col_names [:X :Y :Z] :row_names [:x :y :z]}))
(def emptyMatrix (with-meta [] {:row_cnt 0 :col_cnt 0 :col_names [] :row_names []}))


(deftest matrix?empty-test
  (testing "matrix? thinks [] with meta data is a not matrix"
    (is (= true (util/matrix? emptyMatrix)))))

(deftest matrix?vector-test
  (testing "matrix? thinks [1 2 3 4] w/o meta data is a matrix"
    (is (= false (util/matrix? [1 2 3 4])))))

(deftest matrix?-test
  (testing "matrix? does not think M is a matrix"
    (is (= true (util/matrix? M)))))

(deftest matrix?mismatch-test
  (testing "matrix? thinks a 4 element vector is a 4x4 matrix"
    (is (= false (util/matrix? misMatch)))))


(deftest get-row-test
  (testing "get-row thinks pos 1 is not in row 0 in a 3x2 matrix"
    (is (= 0 (util/get-row M 1)))))

(deftest get-row-max-test
  (testing "get-row thinks pos 5 is not in row 3 in a 3x2 matrix"
    (is (= 2 (util/get-row M 5)))))

(deftest get-col-test
  (testing "get-col thinks pos 1 is not in col 0 in a 3x2 matrix"
    (is (= 1 (util/get-col M 1)))))

(deftest get-col-max-test
  (testing "get-col thinks pos 5 is not in col 2 in a 3x2 matrix"
    (is (= 1 (util/get-col M 5)))))

(deftest first-matrix?-test
  (testing "first-row doesn't return a matrix"
    (is (util/matrix? (util/first-row M)))))

(deftest first-row-even-values-test
  (testing "first-row doesn't return the correct values on an even column matrix"
    (is (= (util/first-row evenSquareMatrix) [14 -8 7 16]))))

(deftest first-row-odd-values-test
  (testing "first-row doesn't return the correct values on an odd column matrix"
    (is (= (util/first-row oddSquareMatrix) [1 8 7]))))

(deftest last-row-matrix?-test
  (testing "last-row doesn't return a matrix"
    (is (util/matrix? (util/last-row M)))))

(deftest last-row-even-test
  (testing "last-row doesn't return the correct values on an even column matrix"
    (is (= (util/last-row evenSquareMatrix) [0 87 2 54]))))

(deftest last-row-odd-test
  (testing "last-row doesn't return the correct values on an odd column matrix"
    (is (= (util/last-row oddSquareMatrix) [42 20 17]))))

(deftest first-col-matrix?-test
  (testing "first-col doesn't return a matrix"
    (is (util/matrix? (util/first-col M)))))

(deftest first-col-even-test
  (testing "first-col doesn't return the correct values on an even column matrix"
    (is (= (util/first-col evenSquareMatrix) [14 32 17 0]))))

(deftest first-col-odd-test
  (testing "first-col doesn't return the correct values on an odd column matrix"
    (is (= (util/first-col oddSquareMatrix) [1 16 42]))))

(deftest last-col-matrix?-test
  (testing "last-col doesn't return a matrix"
    (is (util/matrix? (util/last-col M)))))

(deftest last-col-even-test
  (testing "last-col doesn't return the correct values on an even column matrix"
    (is (= (util/last-col evenSquareMatrix) [16 20 90 54]))))

(deftest last-col-odd-test
  (testing "last-col doesn't return the correct values on an odd column matrix"
    (is (= (util/last-col oddSquareMatrix) [7 9 17]))))

(deftest diagonal-matrix?-test
  (testing "diagonal doesn't return the correct values")
      (is (util/matrix? (util/diagonal M))))

(deftest diagonal-odd-test
  (testing "diagonal doesn't return the correct values")
      (is (= (util/diagonal oddSquareMatrix) [1 32 17])))

(deftest diagonal-even-test
  (testing "diagonal doesn't return the correct values")
      (is (= (util/diagonal evenSquareMatrix) [14 -9 18 54])))











