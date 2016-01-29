(ns matrix.util)

(defn get-row [pos M] (let [col_cnt (:col_cnt (meta M))]
                        (quot pos col_cnt)))

(defn get-col [pos M] (let [col_cnt (:col_cnt (meta M))]
                        (mod pos col_cnt)))

(defn get-pos [[i j] M] (let [col_cnt (:col_cnt (meta M))] 
                        (+ (* i col_cnt) j)))

(defn get-coor [pos M] (list (get-row pos M)  (get-col pos M)))

(defn matrix? [M] 
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (= (* i j) (count M))))

(defn first-row [M] (take (:col_cnt (meta M)) M))
(defn first-col [M] (take-nth (:col_cnt (meta M)) M))
(defn last-row [M] (reverse (take (:col_cnt (meta M)) (reverse M))))
(defn last-col [M] (reverse (take-nth (:col_cnt (meta M)) (reverse M))))
(defn nth-row [n M] (take (:col_cnt (meta M)) (drop (* n (:col_cnt (meta M))) M)))
(defn nth-col [n M] (take-nth (:col_cnt (meta M)) (drop n M)))
(defn diagonal [M] (take-nth (+ 1 (:col_cnt (meta M))) M));left to right diagonal