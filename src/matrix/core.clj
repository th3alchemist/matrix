(ns aIo.matrix.core)

(defn vec-remove
  "takes a sequence coll and index pos.
  Returns the sequence with the element at position pos removed"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn matrix
  "creates an empty matrix. When called with one parameter, it creats an nXn matrix.
  When called with two parameters, it creates an iXj matrix.
  When called with three paraters, it creates an iXj matrix initalized to defaultVal."
  ([i j & [defaultVal]] (with-meta (vec (repeat (* i j) defaultVal))
                      {:row_cnt i
                       :col_cnt j
                       :row_names (vec (map #(keyword (str (char %))) (range 65 (+ 65 i))))
                       :col_names (vec (map #(keyword (str (char %))) (range 97 (+ 97 j))))}))
  ([n] (with-meta (vec (repeat (* n n) nil))
                      {:row_cnt n
                       :col_cnt n
                       :row_names (vec (map #(keyword (str (char %))) (range 65 (+ 65 n))))
                       :col_names (vec (map #(keyword (str (char %))) (range 97 (+ 97 n))))})))

(declare matrix-assoc)

(declare get-pos)

(declare nth-row)

(declare nth-col)

(defn identity-matrix [n]
  (loop [row 0 I (matrix n n 0)]
    (if (= row n)
      I
      (recur
        (inc row)
        (matrix-assoc I
                      (* row (+ n 1))
                      1)))))

(defn matrix? [M]
  (let [i (:row_cnt (meta M))
        j (:col_cnt (meta M))
        row_names (:row_names (meta M))
        col_names (:col_names (meta M))]
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

(defn matrix-assoc
  ([M k v]
  (with-meta (assoc M k v)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))
  ([M row col v] (matrix-assoc M (get-pos M [row col]) v)))

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

(defn get-row [M pos] (quot pos (:col_cnt (meta M))))

(defn get-col [M pos] (mod pos (:col_cnt (meta M))))

(defn get-pos [M [i j]] (+ (* i (:col_cnt (meta M))) j))

(defn get-coor [M pos] [(get-row M pos) (get-col M pos)])

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

(defn nth-row [M n]
  (with-meta (subvec M
                     (* n (:col_cnt (meta M)))
                     (* (inc n) (:col_cnt (meta M))))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) n)
              :col_names (:col_names (meta M))}))

(defn nth-col [M n]
  (with-meta (vec (take-nth (:col_cnt (meta M)) (drop n M)))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt 1
              :row_names (:row_names (meta M))
              :col_names (get (:col_names (meta M)) n)}))

(defn all-rows [M]
  (loop [i 0 out []]
    (if (>= i (:col_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-row M i))))))

(defn all-cols [M]
  (loop [i 0 out []]
    (if (>= i (:col_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-col M i))))))

(defn row [M label]
  ;;get a row by the row name
  (nth-row M (.indexOf (:row_names (meta M)) label)))

(defn col [M label]
  ;;get a column by the column name
  (nth-col M (.indexOf (:col_names (meta M)) label)))

(defn max-ele-length [M]
  (let [rtnVal (inc (apply max (map count (map str M))))]
    (if (some nil? M)
      (max 5 rtnVal)  ;the format function replaces nil with null, which needs five charaters to format  
      rtnVal)))
 
(defn fmt-str [M & [justify _]]
  (str "%"
       ({:left "-" :right ""} justify "-")
       (max-ele-length M)
       "s"))
 
(defn matrix-str [M  & [justify _]]
  (loop [row 0 rtnStr ""]
    (if (= row (:row_cnt (meta M)))
      (clojure.string/trim rtnStr)
      (recur (inc row) (str rtnStr
                            (apply str (map #(format (fmt-str M justify) %) (nth-row M row)))
                             "\n")))))

(defn reflect [M]
  (with-meta (vec (apply concat (map reverse (partition (:col_cnt (meta M)) M))))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))

(defn flip [M]
  (loop [i (dec (:row_cnt (meta M))) out []]
    (if (< i 0)
      (with-meta (vec out) {:row_cnt (:col_cnt (meta M))
                           :col_cnt (:row_cnt (meta M))
                           :row_names (:row_names (meta M))
                           :col_names (:col_names (meta M))})
      (recur (dec i)
             (concat out (nth-row M i))))))

(defn diagonal [M]
  (with-meta (vec (take-nth (+ 1 (:col_cnt (meta M))) M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names []
              :col_names (:col_names(meta M))}));left to right diagonal

(defn anti-diagonal [M]
  (diagonal (reflect M)));right to left diagonal

(defn transpose [M]
  (loop [pos 0 out (matrix (:col_cnt (meta M)) (:row_cnt (meta M)))]
    (if (= pos (* (:row_cnt (meta M)) (:col_cnt (meta M))))
      (with-meta out {:row_cnt (:col_cnt (meta M))
                      :col_cnt (:row_cnt (meta M))
                      :row_names (:row_names (meta M))
                      :col_names (:col_names (meta M))})
      (recur 
        (inc pos)
        (matrix-assoc out
                      (get-pos out [(get-col M pos) (get-row M pos)])
                      (get M pos))))))

(defn drop-nth-row [M row]
  (with-meta (vec (concat (subvec M 0 (* row (:col_cnt (meta M))))
                 (subvec M (* (inc row) (:col_cnt (meta M))))))
             {:row_cnt (dec (:row_cnt (meta M)))
              :col_cnt (:col_cnt (meta M))
              :row_names (vec-remove (:row_names (meta M)) 0)
              :col_names (:col_names (meta M))}))

(defn drop-nth-col [M col]
  (transpose (drop-nth-row (transpose M) col)))

(defn sub-matrix [M [i j]] (drop-nth-row (drop-nth-col M j) i))

(defn row-count [M]
  (:row_cnt (meta M)))

(defn col-count [M]
  (:col_cnt (meta M)))

(defn row-names [M]
  (:row_names (meta M)))

(defn col-names [M]
  (:col_names (meta M)))