(ns matrix.math)
(require 'matrix.util)

(defn scalar-add [M scalar] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map + M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar addition

(defn scalar-sub [M scalar] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map - M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar subtraction
  
(defn scalar-mul [M scalar] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map * M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar multiplication

(defn matrix-add [M N] (with-meta (map + M N){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))}))

(defn matrix-sub [M N] (with-meta (map - M N){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))}))

(defn matrix-mul [M N]
  (loop [pos 0 out (with-meta []
                    {:row_cnt (:row_cnt (meta M)) :col_cnt (:col_cnt (meta N)) :col_names (:col_names (meta M)) :row_names (:row_names (meta N))})]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta N))))
      out
      (recur
        (+ pos 1)
        (assoc (with-meta out {:row_cnt (:row_cnt (meta M)) :col_cnt (:col_cnt (meta N)) :col_names (:col_names (meta M)) :row_names (:row_names (meta N))})
               pos
               (apply + (map *
                             (matrix.util/nth-row (nth (seq (matrix.util/get-coor pos out)) 0) M)
                             (matrix.util/nth-col (nth (seq (matrix.util/get-coor pos out)) 1) N))))))))