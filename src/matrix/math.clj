(ns matrix.math)
(require 'matrix.util)
(alias 'util 'matrix.util)

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
  (loop [pos 0 out (with-meta (util/matrix (:row_cnt (meta N)) (:col_cnt (meta M)))
                    {:row_cnt (:row_cnt (meta M)) :col_cnt (:col_cnt (meta N)) :col_names (:col_names (meta M)) :row_names (:row_names (meta N))})]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta N))))
      out
      (recur
        (+ pos 1)
        (assoc (with-meta out {:row_cnt (:row_cnt (meta M)) :col_cnt (:col_cnt (meta N)) :col_names (:col_names (meta M)) :row_names (:row_names (meta N))})
               pos
               (apply + (map *
                             (util/nth-row M (nth (seq (util/get-coor out pos)) 0))
                             (util/nth-col N (nth (seq (util/get-coor out pos)) 1)))))))))

(defn matrix-not [M]
  (loop [pos 0 out (with-meta (util/matrix (:row_cnt (meta M))
                                           (:col_cnt (meta M)))
                              {:row_cnt (:row_cnt (meta M))
                               :col_cnt (:col_cnt (meta M))
                               :col_names (:col_names (meta M))
                               :row_names (:row_names (meta M))})]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (assoc out pos (not (get M pos)))))))

(defn matrix-and [M N]
 (loop [pos 0 out (with-meta (util/matrix (:row_cnt (meta M))
                                          (:col_cnt (meta M)))
                              {:row_cnt (:row_cnt (meta M))
                               :col_cnt (:col_cnt (meta M))
                               :col_names (:col_names (meta M))
                               :row_names (:row_names (meta M))})]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (assoc out pos (and (get M pos) (get N pos)))))))

(defn matrix-or [M N]
  (loop [pos 0 out (with-meta (util/matrix (:row_cnt (meta M))
                                           (:col_cnt (meta M)))
                              {:row_cnt (:row_cnt (meta M))
                               :col_cnt (:col_cnt (meta M))
                               :col_names (:col_names (meta M))
                               :row_names (:row_names (meta M))})]
    (if (= pos (count M))
      out
      (recur
        (inc pos)
        (assoc out pos (or (get M pos) (get N pos)))))))

(defn trace [M] (apply + (util/diagonal M)))

(defn det [M] (if (util/square-matrix?) M))