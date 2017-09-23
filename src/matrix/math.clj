(ns aIo.matrix.math)
(require 'aIo.matrix.core)
(alias 'mCore 'aIo.matrix.core)

(defn scalar-add
  "adds scalar to every element of matrix M"
  [M scalar]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map + M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})))
  
(defn scalar-mul
  "multiplies every element of M by value scalar"
  [M scalar]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (vec (map * M (repeat (* i j) scalar))){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})))

(defn matrix-add
  "adds each element of matrix M to the corresponding element in matrix N"
  [M N]
  (with-meta (map + M N){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))}))

(defn matrix-mul
  "multiplies matrix M by matrix N"
  [M N]
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M)) (:col_cnt (meta N)))]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta N))))
      out
      (recur
        (+ pos 1)
        (mCore/matrix-assoc out
               pos
               (apply + (map *
                             (mCore/nth-row M (nth (mCore/get-coor out pos) 0))
                             (mCore/nth-col N (nth (mCore/get-coor out pos) 1)))))))))

(defn matrix-not
  "applies a logical not to every element of matrix M"
  [M]
    (with-meta (map not M){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))}))

(defn matrix-and
  "logical ands M with N"
  [M N]
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                 (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (and (get M pos) (get N pos)))))))

(defn matrix-or
  "logical ands M with N"
  [M N]
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                 (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (or (get M pos) (get N pos)))))))

(defn trace
  "calculates the trace of matrix M"
  [M]
  (apply + (mCore/diagonal M)))