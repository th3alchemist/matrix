(ns matrix.graph-test
  (:require [clojure.test :refer :all]
            [matrix.core :refer :all]))
(alias 'mCore 'matrix.core)

(def hyper-cube (with-meta
  [0   1   1   nil 1   nil nil nil 2   nil nil nil nil nil nil nil ;a
   1   0   nil 1   nil 1   nil nil nil 2   nil nil nil nil nil nil ;b
   1   nil 0   1   nil nil 1   nil nil nil 2   nil nil nil nil nil ;c
   nil 1   1   0   nil nil nil 1   nil nil nil 2   nil nil nil nil ;d
   1   nil nil nil 0   1   1   nil nil nil nil nil 2   nil nil nil ;e
   nil 1   nil nil 1   0   nil 1   nil nil nil nil nil 2   nil nil ;f
   nil nil 1   nil 1   nil 0   1   nil nil nil nil nil nil 2   nil ;g
   nil nil nil 1   nil 1   1   0   nil nil nil nil nil nil nil 2   ;h
   2   nil nil nil nil nil nil nil 0   1   1   nil 1   nil nil nil ;a*
   nil 2   nil nil nil nil nil nil 1   0   nil 1   nil 1   nil nil ;b*
   nil nil 2   nil nil nil nil nil 1   nil 0   1   nil nil 1   nil ;c*
   nil nil nil 2   nil nil nil nil nil 1   1   0   nil nil nil 1   ;d*
   nil nil nil nil 2   nil nil nil 1   nil nil nil 0   1   1   nil ;e*
   nil nil nil nil nil 2   nil nil nil 1   nil nil 1   0   nil 1   ;f*
   nil nil nil nil nil nil 2   nil nil nil 1   nil 1   nil 0   1   ;g*
   nil nil nil nil nil nil nil 2   nil nil nil 1   nil 1   1   0  ];h*
  {:row_cnt 16
   :col_cnt 16
   :row_names [:a :b :c :d :e :f :g :h :a* :b* :c* :d* :e* :f* :g* :h*]
   :col_names [:a :b :c :d :e :f :g :h :a* :b* :c* :d* :e* :f* :g* :h*]}))

(def binary-tree (with-meta [nil 1   1   nil nil nil nil  ;a
                             nil nil nil 1   1   nil nil  ;b
                             nil nil nil nil nil 1   1    ;c
                             nil nil nil nil nil nil nil  ;d
                             nil nil nil nil nil nil nil  ;e
                             nil nil nil nil nil nil nil  ;f
                             nil nil nil nil nil nil nil] ;g
                             {:row_cnt 7, :col_cnt 7, :row_names [:a :b :c :d :e :f :g], :col_names [:a :b :c :d :e :f :g]}))


(def random-graph (with-meta [nil nil nil 1   1   1   ;a
                              nil nil nil 1   nil nil ;b
                              nil nil nil nil nil 1   ;c
                              1   1   nil nil 1   nil ;d
                              1   nil nil 1   nil nil ;e
                              1   nil 1   nil nil nil];f
                             {:row_cnt 6
                              :col_cnt 6
                              :row_names [:a :b :c :d :e :f]
                              :col_names [:a :b :c :d :e :f]}))