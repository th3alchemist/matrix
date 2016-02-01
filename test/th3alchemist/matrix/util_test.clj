(ns matrix.util-test
  (:require [clojure.test :refer :all]
            [matrix.util :refer :all]))


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

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
