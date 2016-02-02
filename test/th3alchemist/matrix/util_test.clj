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
(def P (with-meta [29 84 -20 37]
                           {:row_cnt 1 :col_cnt 4 :col_names [:age] :row_names [:shayla :zamirh :codex :josier]}))
(def Q (with-meta [8.406 1.553 0.622 0.447]
                           {:row_cnt 4 :col_cnt 1 :col_names [:nyc :phl :stl :atl] :row_names [:population]}))
(def misMatch (with-meta [8.406 1.553 0.622 0.447]
                           {:row_cnt 4 :col_cnt 4 :col_names [:nyc :phl :stl :atl] :row_names [:population]}))


(deftest matrix?empty-test
  (testing "matrix? thinks [] w/o meta data is a matrix"
    (is (= false (util/matrix? [])))))

(deftest matrix?vector-test
  (testing "matrix? thinks [1 2 3 4] w/o meta data is a matrix"
    (is (= false (util/matrix? [1 2 3 4])))))

(deftest matrix?-test
  (testing "matrix? does not think M is a matrix"
    (is (= true (util/matrix? M)))))

(deftest matrix?mismatch-test
  (testing "matrix? thinks a 4 element vector is a 4x4 matrix"
    (is (= false (util/matrix? misMatch)))))

(deftest get-row-negative-test
  (testing "get-row allows negative positions"
    (is (= nil (util/get-row M -2)))))

(deftest get-row-test
  (testing "get-row thinks pos 1 is not in row 0 in a 3x2 matrix"
    (is (= 0 (util/get-row M 1)))))

(deftest get-row-max-test
  (testing "get-row thinks pos 5 is not in row 3 in a 3x2 matrix"
    (is (= 2 (util/get-row M 5)))))

(deftest get-row-upperbounds-test
  (testing "get-row thinks pos 6 is in a 3x2 matrix"
    (is (= nil (util/get-row M 6)))))


(deftest get-col-negative-test
  (testing "get-col allows negative positions"
    (is (= nil (util/get-col M -2)))))

(deftest get-col-test
  (testing "get-col thinks pos 1 is not in col 0 in a 3x2 matrix"
    (is (= 1 (util/get-col M 1)))))

(deftest get-col-max-test
  (testing "get-col thinks pos 5 is not in col 2 in a 3x2 matrix"
    (is (= 1 (util/get-col M 5)))))

(deftest get-col-upperbounds-test
  (testing "get-row thinks pos 6 is in a 3x2 matrix"
    (is (= nil (util/get-col M 6)))))













