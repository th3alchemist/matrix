(ns matrix.math)
(require 'matrix.util)

(defn scalar-add [scalar M] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map + M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar addition

(defn scalar-sub [scalar M] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map - M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar subtraction
  
  
(defn scalar-mul [scalar M] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
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

(defn multiply [M N]
  (loop [pos 0 out (vec (repeat (* 
                                 (:row_cnt (meta O))
                                 (:col_cnt (meta O)))
                                nil))]
    (if (> pos 8)
      out
      (recur
        (+ pos 1)
        (assoc out
               pos
               (apply + (map *
                             (nth-row (nth (seq (get-coor pos O)) 0) O)
                             (nth-col (nth (seq (get-coor pos O)) 1) O)
               )))
        ))))

;;(apply + (map * (matrix.util/nth-row i M) (matrix.util/nth-col j N)))

















