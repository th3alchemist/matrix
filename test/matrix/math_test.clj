(ns matrix.math-test
  (:require [clojure.test :refer :all]
            [matrix.util :refer :all]))
(alias 'util 'matrix.util)


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

;;test-order
;;1 - does it return a matrix
;;2 - does it return the correct values on a 2x3
;;3 - does it return the correct values on an odd squared 
;;4 - does it return the correct values on an even square matrices
;;5 - does it accept an empty matrix