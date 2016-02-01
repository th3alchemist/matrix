(ns th3alchemist.matrix.util)

(defn get-row [M pos] (if (< -1 pos (count M))
                        (do 
                          (let [col_cnt (:col_cnt (meta M))]
                            (quot pos col_cnt)))))

(defn get-col [M pos] (if (< -1 pos (count M))
                        (do 
                          (let [col_cnt (:col_cnt (meta M))]
                        (mod pos col_cnt)))))

(defn get-pos [M [i j]] (if (and (< -1 i (:row_cnt (meta M)))
                                 (< -1 j (:col_cnt (meta M))))
                        (+ (* i (:col_cnt (meta M))) j)))

(defn get-coor [M pos] (if (< -1 pos (count M))
                         (list (get-row M pos)  (get-col M pos))))

(defn matrix? [M]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (= (* i j) (count M))))

(defn square-matrix? [M]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (and (= (* i j) (count M))
         (= i j))))

(defn first-row [M] (take (:col_cnt (meta M)) M))
(defn first-col [M] (take-nth (:col_cnt (meta M)) M))
(defn last-row [M] (reverse (take (:col_cnt (meta M)) (reverse M))))
(defn last-col [M] (reverse (take-nth (:col_cnt (meta M)) (reverse M))))
(defn nth-row [M n] (if (< -1 n (:row_cnt (meta M)))
                      (take (:col_cnt (meta M)) (drop (* n (:col_cnt (meta M))) M))))
(defn nth-col [M n] (if (< -1 n (:col_cnt (meta M)))
                      (take-nth (:col_cnt (meta M)) (drop n M))))
(defn diagonal [M] (take-nth (+ 1 (:col_cnt (meta M))) M));left to right diagonal


(defn identity-matrix [n] (repeat n 0))