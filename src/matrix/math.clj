(ns matrix.math)
(require 'matrix.core)
(alias 'mCore 'matrix.core)

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
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M)) (:col_cnt (meta N)))]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta N))))
      out
      (recur
        (+ pos 1)
        (mCore/matrix-assoc out
               pos
               (apply + (map *
                             (mCore/nth-row M (nth (seq (mCore/get-coor out pos)) 0))
                             (mCore/nth-col N (nth (seq (mCore/get-coor out pos)) 1)))))))))

(defn matrix-not [M]
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                 (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (not (get M pos)))))))

(defn matrix-and [M N]
 (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (and (get M pos) (get N pos)))))))

(defn matrix-or [M N]
  (loop [pos 0 out (mCore/matrix (:row_cnt (meta M))
                                 (:col_cnt (meta M)))]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (mCore/matrix-assoc out pos (or (get M pos) (get N pos)))))))

(defn trace [M] (apply + (mCore/diagonal M)))

(defn det [M]
  (loop [col 0 matrix M]
    (if (= col (:col_cnt (meta matrix)));;(= 4 (count M))
      (- (* (nth matrix 0)
            (nth matrix 3))
         (* (nth matrix 1)
            (nth matrix 2)))
      (recur (inc col) (det (mCore/sub-matrix matrix (vector col col)))))))

(def det2 [M]
  (loop [matrix M col 0 acc 0]
    (if (= 4 (count M))
      acc
      (recur (sub-matrix M (vector col col)) (inc col) (inc acc)))))
  









