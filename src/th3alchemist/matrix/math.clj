(ns th3alchemist.matrix.math)
(require 'th3alchemist.matrix.util)

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

(defn matrix-mul [M N] ;; broken b/c get-coor rtns nil
  (loop [pos 0 out (with-meta (vec (repeat (* (:row_cnt (meta M)) (:col_cnt (meta N))) nil))
                    {:row_cnt (:row_cnt (meta M)) :col_cnt (:col_cnt (meta N)) :col_names (:col_names (meta M)) :row_names (:row_names (meta N))})]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta N))))
      out
      (recur
        (+ pos 1)
        (assoc (with-meta out {:row_cnt (:row_cnt (meta M)) :col_cnt (:col_cnt (meta N)) :col_names (:col_names (meta M)) :row_names (:row_names (meta N))})
               pos
               (apply + (map *
                             (th3alchemist.matrix.util/nth-row M (nth (seq (th3alchemist.matrix.util/get-coor out pos)) 0))
                             (th3alchemist.matrix.util/nth-col N (nth (seq (th3alchemist.matrix.util/get-coor out pos)) 1)))))))))

(defn transpose [M] "hi")
(defn matrix-and [M N] "hi")
(defn matrix-or [M N] "hi")
(defn matrix-not [M] "hi")
(defn submatrix [i j M] "hi")
;;add keyword bindings