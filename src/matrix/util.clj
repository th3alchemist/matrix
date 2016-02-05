(ns matrix.util)

(defn vec-remove
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn matrix
  ([i j] (with-meta (vec (repeat (* i j) nil))
                      {:row_cnt i
                       :col_cnt j
                       :row_names (vec (range i))
                       :col_names (vec (range j))}))
  ([n] (with-meta (vec (repeat (* n n) nil))
                      {:row_cnt n
                       :col_cnt n
                       :row_names (vec (range n))
                       :col_names (vec (range n))})))

(defn identity-matrix [n]
  (loop [row 0 I (with-meta (vec (repeat (* n n) 0))
                           {:row_cnt n
                            :col_cnt n
          	                :row_names (vec (range n))
                            :col_names (vec (range n))})]
    (if (= row n) 
      I
      (recur 
        (inc row) 
        (assoc (with-meta I {:row_cnt n
                             :col_cnt n
                             :row_names (vec (range n))
                             :col_names (vec (range n))})
          (+ row (* row n))
          1)))))

(defn matrix? [M]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M)) row_names (:row_names (meta M)) col_names (:col_names (meta M))]
    (if (or (nil? i)
            (nil? j)
            (nil? row_names)
            (nil? col_names))
      false
      (and (= (* i j) (count M))
           (vector? M)))))

(defn square-matrix? [M]
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M)) row_names (:row_names (meta M)) col_names (:col_names (meta M))]
    (and (matrix? M)
         (= i j))))

(defn equal? [M N]
  (and (= M N)
       (= (:row_cnt (meta M))
          (:row_cnt (meta N)))
       (= (:col_cnt (meta M))
          (:col_cnt (meta N)))))

(defn get-row [M pos] (quot pos (:col_cnt (meta M))))

(defn get-col [M pos] (mod pos (:col_cnt (meta M))))

(defn get-pos [M [i j]] (+ (* i (:col_cnt (meta M))) j))

(defn get-coor [M pos] (vec (get-row M pos) (get-col M pos)))

(defn first-row [M]
  (with-meta (take (:col_cnt (meta M)) M)
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) 0)
              :col_names (:col_names (meta M))}))

(defn first-col [M]
  (with-meta (take-nth (:col_cnt (meta M)) M)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt 1
              :row_names (:row_names (meta M))
              :col_names (get (:col_names (meta M)) 0)}))

(defn last-row [M]
  (with-meta (subvec M
                     (- (count M) (:col_cnt (meta M)))
                     (count M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) (dec (count (:row_names (meta M)))))
              :col_names (:col_names (meta M))}))

(defn last-col [M]
  (with-meta (reverse (take-nth (:col_cnt (meta M)) (reverse M)))
                              {:row_cnt (:row_cnt (meta M))
                               :col_cnt 1
                               :row_names (:row_names (meta M))
                               :col_names (get (:col_names (meta M)) (dec (count (:col_names (meta M)))))}))

(defn nth-row [M row]
    (with-meta (subvec M
                       (* row (:col_cnt (meta M)))
                       (* (inc row) (:col_cnt (meta M))))
               {:row_cnt 1
                :col_cnt (:col_cnt (meta M))
                :row_names (get (:row_names (meta M)) row)
                :col_names (:col_names (meta M))}))

(defn nth-col [M col]
  (with-meta (take-nth (:col_cnt (meta M)) (drop col M))
                               {:row_cnt (:row_cnt (meta M))
                                :col_cnt 1
                               :row_names (:row_names (meta M))
                               :col_names (get (:col_names (meta M)) col)}))

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
        (conj out (nth-col M i))))))

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
                    :row_names (:row_names (meta M))
                    :col_names (reverse (:col_names (meta M)))})))))

(defn diagonal [M]
  (with-meta (take-nth (+ 1 (:col_cnt (meta M))) M)
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names []
              :col_names (:col_names(meta M))}));left to right diagonal

(defn anti-diagonal [M]
  (with-meta (take-nth (+ 1 (:col_cnt (meta M))) (flip M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names []
              :col_names (reverse (:col_names(meta M)))}));right to left diagonal

(defn transpose [M]
  (loop [pos 0 out (with-meta (matrix (:row_cnt (meta M)) (:col_cnt (meta M))) 
                    {:row_cnt (:col_cnt (meta M))
                     :col_cnt (:row_cnt (meta M))
                     :col_names (:row_names (meta M))
                     :row_names (:col_names (meta M))})]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta M))))
      out
      (recur 
        (inc pos)
        (assoc (with-meta out {:row_cnt (:col_cnt (meta M))
                               :col_cnt (:row_cnt (meta M))
                               :col_names (:row_names (meta M))
                               :row_names (:col_names (meta M))}) 
          (get-pos out [(get-col M pos) (get-row M pos)])
          (get M pos))))))

(defn drop-nth-row [M row]
    (with-meta (vec (concat (subvec M 0 (* row (:col_cnt (meta M)))) (subvec M (* (inc row) (:col_cnt (meta M))))))
               {:row_cnt (dec (:row_cnt (meta M)))
                :col_cnt (:col_cnt (meta M))
                :row_names (vec-remove (:row_names (meta M)) row)
                :col_names (:col_names (meta M))}))

(defn drop-nth-col [M col]
    (with-meta (transpose (drop-nth-row (transpose M) col))
               {:row_cnt (:row_cnt (meta M))
                :col_cnt (dec (:col_cnt (meta M)))
                :row_names (:row_names (meta M))
                :col_names (vec-remove (:col_names (meta M)) col)}))

(defn sub-matrix [M [i j]] (drop-nth-row (drop-nth-col M j) i))

(defn matrix-str [M]
  (if (not (matrix? M)) nil)
  (loop [pos 0 rtnStr ""]
    (if (= pos (:row_cnt (meta M)))
      rtnStr
      (recur (inc pos) (str rtnStr (seq (nth-row M pos)) "\n")))))

(defn matrix-assoc [M k v]
  (with-meta (assoc M k v)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (dec (:col_cnt (meta M)))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))
