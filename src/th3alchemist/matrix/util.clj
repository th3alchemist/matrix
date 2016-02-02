(ns th3alchemist.matrix.util)

(defn get-row [M pos] (if (< -1 pos (count M))
                        (quot pos (:col_cnt (meta M)))))

(defn get-col [M pos] (if (< -1 pos (count M))
                        (mod pos (:col_cnt (meta M)))))

(defn get-pos [M [i j]] (if (and (< -1 i (:row_cnt (meta M)))
                                 (< -1 j (:col_cnt (meta M))))
                          (+ (* i (:col_cnt (meta M))) j)))

(defn get-coor [M pos] (if (< -1 pos (count M))
                         (list (get-row M pos) (get-col M pos))))

(defn matrix
  ([i j] (with-meta (vec (repeat (* i j) nil))
                      {:row_cnt i :col_cnt j}))
  ([n] (with-meta (vec (repeat (* n n) nil))
                      {:row_cnt n :col_cnt n})))

(defn matrix? [M]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (if (or (nil? i)
            (nil? j))
      false
      (= (* i j) (count M)))))

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

(defn all-rows [M]
  (loop [i 0 out []]
    (if (>= i (:col_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-row i M))))))

(defn all-cols [M]
  (loop [i 0 out []]
    (if (>= i (:col_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-col i M))))))

(defn diagonal [M] (take-nth (+ 1 (:col_cnt (meta M))) M));left to right diagonal

(defn anti-diagonal [M]
  (with-meta (take-nth (+ 1 (:col_cnt (meta M))) (flip M))
                              {:row_cnt 1 :col_cnt (:col_cnt (meta M))}));right to left diagonal

(defn matrix-str [M]
  (if (not (matrix? M)) nil)
  (loop [pos 0 rtnStr ""]
    (if (= pos (:row_cnt (meta M)))
      rtnStr
      (recur (inc pos) (str rtnStr (seq (nth-row pos M)) "\n")))))

(defn identity-matrix [n]
  (loop [row 0 I (with-meta (vec (repeat (* n n) 0))
                           {:row_cnt n :col_cnt n})]
    (if (= row n) 
      I
      (recur 
        (inc row) 
        (assoc (with-meta I {:row_cnt n
                             :col_cnt n}) 
          (+ row (* row n))
          1)))))

(defn equal? [M N]
  (and (= M N)
       (= (:row_cnt (meta M))
          (:row_cnt (meta N)))
       (= (:col_cnt (meta M))
          (:col_cnt (meta N)))))

(defn flip [matrix]
  (loop [row 0 M matrix]
    (if (> row (:row_cnt (meta M)))
      M
      (recur
        (inc row)
        (with-meta (vec (concat (take (* row (:col_cnt (meta M))) M);;everything before the nth row
                                (reverse (nth-row row M)) ;;reverse the nth row
                                (reverse (take  (- (count M) (* (inc row) (:col_cnt (meta M)))) (reverse M)))));;everything after the nth row
                   {:row_cnt (:row_cnt (meta M))
                    :col_cnt (:col_cnt (meta M))
                    :col_names (:col_names (meta M))
                    :row_names (:row_names (meta M))})))))