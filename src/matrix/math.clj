(ns matrix.math)
(require 'matrix.core)
(alias 'mCore 'matrix.core)

(defn scalar-add [M n]
  "Accepts a matrix M and a number n. Returns a M with n added to each value"
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map + M (repeat (* i j) n)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})))
  
(defn scalar-mul [M scalar]
  "Accepts a matrix M and a number n. Returns a M with n multiplied by each value"
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (vec (map * M (repeat (* i j) scalar))){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})))

(defn matrix-add [M N]
  "Accepts two matricies M and N. Returns M with
  each corresponding element in matrix N added to it"
  (with-meta (map + M N){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))}))

(defn matrix-mul [M N]
  "Accepts two matricies M and N. Returns M multiplied by matrix N"
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

(defn matrix-not [M]
  "applies a logical not to every element of matrix M"
  (with-meta (map not M){
             :row_cnt (:row_cnt (meta M))
             :col_cnt (:col_cnt (meta M))
             :col_names (:col_names (meta M))
             :row_names (:row_names (meta M))}))

(defn matrix-and [M N]
  "logical ands M with N"
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                 (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (and (get M pos) (get N pos)))))))

(defn matrix-or [M N]
  "logical ands M with N"
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                 (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (or (get M pos) (get N pos)))))))

(defn trace [M]
  "calculates the trace of matrix M"
  (apply + (mCore/diagonal M)))