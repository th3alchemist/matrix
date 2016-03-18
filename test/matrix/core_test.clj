(ns matrix.core-test
  (:require [clojure.test :refer :all]
            [matrix.core :refer :all]))
(alias 'mCore 'matrix.core)

(def M (with-meta [98 4 2 3 7 0]
                           {:row_cnt 3 :col_cnt 2 :col_names [:A :B :C] :row_names [:A :B]}))
(def N (with-meta [0 5 23 19 69 13]
                           {:row_cnt 2 :col_cnt 3 :col_names [:a :b] :row_names [:a :b :c]}))
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

(def listMatrix (with-meta '(98 4 2 3 7 0)
                           {:row_cnt 3 :col_cnt 2 :col_names [:A :B :C] :row_names [:A :B]}))
(def booleanMatrix (with-meta [true true false false true false]
                           {:row_cnt 3 :col_cnt 2 :col_names [:A :B :C] :row_names [:A :B]}))

;;test-order
;;1 - does it return a matrix
;;2 - does it return the correct values on a 2x3
;;3 - does it return the correct values on an odd squared 
;;4 - does it return the correct values on an even square matrices
;;5 - does it accept an empty matrix

(deftest matrix?-test
  (testing "matrix? thinks [] with meta data is a not matrix"
    (is (= true (mCore/matrix? emptyMatrix)))))

(deftest matrix?-list-test
  (testing "matrix? thinks a list is not matrix"
    (is (= false (mCore/matrix? listMatrix)))))

(deftest matrix?empty-test
  (testing "matrix? thinks [] with meta data is a not matrix"
    (is (= true (mCore/matrix? emptyMatrix)))))

(deftest matrix?vector-test
  (testing "matrix? thinks [1 2 3 4] w/o meta data is a matrix"
    (is (= false (mCore/matrix? [1 2 3 4])))))

(deftest matrix?-test
  (testing "matrix? does not think M is a matrix"
    (is (= true (mCore/matrix? M)))))

(deftest matrix?mismatch-test
  (testing "matrix? thinks a 4 element vector is a 4x4 matrix"
    (is (= false (mCore/matrix? misMatch)))))



(deftest square-matrix?-odd-test
  (testing "square-matrix returns false for an oddSquareMatrix"
    (is (= true (mCore/square-matrix? oddSquareMatrix)))))

(deftest square-matrix?-even-test
  (testing "square-matrix returns false for an evenSquareMatrix"
    (is (= true (mCore/square-matrix? evenSquareMatrix)))))

(deftest square-matrix?-empty-test
  (testing "square-matrix returns false for an emptyMatrix"
    (is (= true (mCore/square-matrix? emptyMatrix)))))

(deftest matrix-square?-test
  (testing "(matrix n) does not return a square matrix"
    (is (mCore/square-matrix? (mCore/matrix 4)))))




(deftest matrix-test
  (testing "(matrix i j) does not return a matrix"
    (is (mCore/matrix? (mCore/matrix 3 4)))))

(deftest matrix-row-test
  (testing "(matrix i j) does not return a matrix with i rows"
    (is (= 3 (:row_cnt (meta (mCore/matrix 3 4)))))))

(deftest matrix-col-test
  (testing "(matrix i j) does not return a matrix with j columns"
    (is (= 4 (:col_cnt (meta (mCore/matrix 3 4)))))))

(deftest matrix-count-test
  (testing "(matrix i j) does not return a matrix with i*j elements"
    (is (= 12 (count (mCore/matrix 3 4))))))



















