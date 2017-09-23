(ns matrix.graphics-test
  (:require [clojure.test :refer :all]
            [matrix.core :refer :all]))
(alias 'mCore 'matrix.core)



(def a (with-meta [0 1]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))
(def b (with-meta [-1 0]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))
(def c (with-meta [1 0]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))

(def triangle [a b c])


(def w (with-meta [-1 1]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))
(def x (with-meta [1 1]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))
(def y (with-meta [1 -1]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))
(def z (with-meta [-1 -1]
                           {:row_cnt 2 :col_cnt 1 :row_names [:x :y] :col_names [:A]}))

(def square [w x y z])

(def scale (with-meta [2 0 0 2]
                           {:row_cnt 2 :col_cnt 2 :row_names [:x :y] :col_names [:A :B]}))