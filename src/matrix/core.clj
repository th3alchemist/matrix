(ns matrix.core)

(defn vec-remove
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn matrix
  ([i j & [defaultVal]] (with-meta (vec (repeat (* i j) defaultVal))
                      {:row_cnt i
                       :col_cnt j
                       :row_names (vec (range i))
                       :col_names (vec (range j))}))
  ([n] (with-meta (vec (repeat (* n n) nil))
                      {:row_cnt n
                       :col_cnt n
                       :row_names (vec (range n))
                       :col_names (vec (range n))})))

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
  (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (and (matrix? M)
         (= i j))))

(defn equal? [M N]
  (and (= M N)
       (= (:row_cnt (meta M))
          (:row_cnt (meta N)))
       (= (:col_cnt (meta M))
          (:col_cnt (meta N)))))

(declare nth-row)

(defn max-token-count [board]
  ;the format function replaces nil with null, which needs five charaters to format
  (apply max (conj (map count (map str board)) 5)))

(defn fmt-str [board & [justify _]]
  (str "%"
       ({:left "-" :right ""} justify "-")
       (max-token-count board)
       "s"))

(defn matrix-str [M  & [justify _]]
  (loop [row 0 rtnStr ""]
    (if (= row (:row_cnt (meta M)))
      rtnStr
      (recur (inc row) (str rtnStr
                            (apply str (map #(format (fmt-str M justify) %1) (nth-row M row)))
                            "\n")))))

(defn matrix-assoc [M k v]
  (with-meta (assoc M k v)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))

(defn matrix-concat [M N]
  (with-meta (vec (concat M N))
             {:row_cnt (+ (:row_cnt (meta M)) (:row_cnt (meta N)))
              :col_cnt (:col_cnt (meta M))
              :row_names (vec (concat (:row_names (meta M)) (:row_names (meta N))))
              :col_names (:col_names (meta M))}))

(defn matrix-conj [M N]
  (loop [out [] row 0]
    (if (= row (:row_cnt (meta M)))
      out
      (recur (with-meta (vec (concat out (nth-row M row) (nth-row N row)))
                        {:row_cnt (:row_cnt (meta M))
                         :col_cnt (+ (:col_cnt (meta M)) (:col_cnt (meta N)))
                         :row_names (:row_names (meta M))
                         :col_names (vec (concat (:col_names (meta M)) (:col_names (meta N))))})
              (inc row)))))

(defn identity-matrix [n]
  (loop [row 0 I (matrix n)]
    (if (= row n) 
      I
      (recur
        (inc row)
        (matrix-assoc I
                      (* row (+ n 1))
                      1)))))

(defn get-row [M pos] (quot pos (:col_cnt (meta M))))

(defn get-col [M pos] (mod pos (:col_cnt (meta M))))

(defn get-pos [M [i j]] (+ (* i (:col_cnt (meta M))) j))

(defn get-coor [M pos] (vector (get-row M pos) (get-col M pos)))

(defn row [M label]
  (nth-row M (.indexOf (:row_names (meta M)) label)))

(defn col [M label]
  (nth-col M (.indexOf (:col_names (meta M)) label)))

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
    (if (= row (:row_cnt (meta M)))
      M
      (recur
        (inc row)
        (with-meta (vec (concat (take (* row (:col_cnt (meta M))) M);;everything before the nth row
                                (reverse (nth-row M row)) ;;reverse the nth row
                                (take-last (- (count M) (* (inc row) (:col_cnt (meta M)))) M)));;everything after the nth row
                   {:row_cnt (:row_cnt (meta M))
                    :col_cnt (:col_cnt (meta M))
                    :row_names (:row_names (meta M))
                    :col_names (:col_names (meta M))})))))

(defn diagonal [M]
  (with-meta (take-nth (+ 1 (:col_cnt (meta M))) M)
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names []
              :col_names (:col_names(meta M))}));left to right diagonal

(defn anti-diagonal [M]
  (diagonal (flip M)));right to left diagonal

(defn transpose [M]
  (loop [pos 0 out (matrix (:col_cnt (meta M)) (:row_cnt (meta M)))]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta M))))
      (with-meta out {:row_cnt (:col_cnt (meta M))
                      :col_cnt (:row_cnt (meta M))
                      :row_names (:row_names (meta M))
                      :col_names (:col_names (meta M))
                      })
      (recur 
        (inc pos)
        (matrix-assoc out
                      (get-pos out [(get-col M pos) (get-row M pos)])
                      (get M pos))))))

(defn drop-nth-row [M row]
  (with-meta (vec (concat (subvec M 0 (* row (:col_cnt (meta M))))
                 (subvec M (* (inc row) (:col_cnt (meta M))))))
             {:row_cnt (-  (:row_cnt (meta M)) 1)
              :col_cnt (:col_cnt (meta M))
              :row_names (vec-remove (:row_names (meta M)) 0)
              :col_names (:col_names (meta M))}))

(defn drop-nth-col [M col]
  (transpose (drop-nth-row (transpose M) col)))

(defn sub-matrix [M [i j]] (drop-nth-row (drop-nth-col M j) i))






















