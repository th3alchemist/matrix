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
  (loop [pos 0 out (with-meta (vec (repeat (* (:row_cnt (meta M))
                                              (:col_cnt (meta N)))
                              nil))
                    {:row_cnt 3 :col_cnt 3 :col_names [:a :b] :row_names [:a :b :c]})]
    (if (> pos (* (:row_cnt (meta M)) (:col_cnt (meta N))))
      out
      (recur
        (+ pos 1)
        (assoc (with-meta out {:row_cnt 3 :col_cnt 3 :col_names [:a :b] :row_names [:a :b :c]})
               pos
               (apply + (map *
                             (matrix.util/nth-row (nth (seq (matrix.util/get-coor pos out)) 0) M)
                             (matrix.util/nth-col (nth (seq (matrix.util/get-coor pos out)) 1) N))))))))

;;(apply + (map * (matrix.util/nth-row i M) (matrix.util/nth-col j N)))












