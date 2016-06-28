(ns matrix.core)

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
  "Takes an integer n and returns a square
  matrix with all zeros, except on the
  main diagonal when the values are one."
  (loop [row 0 I (matrix n n 0)]
    (if (= row n)
      I
      (recur
        (inc row)
        (matrix-assoc I
                      (* row (+ n 1))
                      1)))))

(defn matrix? [M]
  "Accepts a collection M and returns true
  if it is a matrix; ie. is a vector
  with i(row count) j(column count)
  and row/col name meta data, and the
  length of the collection is equal to i times j"
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
  "Accepts a collection M and returns true
  if it is a matrix and if the number
  of rows equals the number of columns."
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
  "assoc function wrapper. Accepts
  the same args as assoc, M(matrix-str)
  k(key, index) and v(value) but returns
  a matrix, ie vector with meta-data"
  ([M k v]
  (with-meta (assoc M k v)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))
  ([M row col v] (matrix-assoc M (get-pos M [row col]) v)))

(defn matrix-concat [M N]
  "Accepts two matrcies M and N, and returns matrix N appended to the bottom of matrix M"
  (with-meta (vec (concat M N))
             {:row_cnt (+ (:row_cnt (meta M)) (:row_cnt (meta N)))
              :col_cnt (:col_cnt (meta M))
              :row_names (vec (concat (:row_names (meta M)) (:row_names (meta N))))
              :col_names (:col_names (meta M))}))

(defn matrix-conj [M N]
  "Accepts two matrcies M and N,
  and returns matrix N appended
  to the left of matrix M"
  (loop [out [] row 0]
    (if (= row (:row_cnt (meta M)))
      out
      (recur (with-meta (vec (concat out (nth-row M row) (nth-row N row)))
                        {:row_cnt (:row_cnt (meta M))
                         :col_cnt (+ (:col_cnt (meta M)) (:col_cnt (meta N)))
                         :row_names (:row_names (meta M))
                         :col_names (vec (concat (:col_names (meta M)) (:col_names (meta N))))})
              (inc row)))))

(defn get-row [M pos]
  "Accepts a matrix M and index pos
  and returns the row number
  containing the value in position pos"
  (quot pos (:col_cnt (meta M))))

(defn get-col [M pos]
  "Accepts a matrix M and index pos
  and returns the col number
  containing the value in position pos"
  (mod pos (:col_cnt (meta M))))

(defn get-pos [M [i j]]
  "Accepts a matrix M and a collection
  containing a row index and column
  index. Returns the index of the coordinate."
  (+ (* i (:col_cnt (meta M))) j))

(defn get-coor [M pos]
  "Accepts a matrix M and position pos.
  Returns a vector with the corresponding
  row and column."
  [(get-row M pos) (get-col M pos)])

(defn first-row [M]
  "Accepts a matrix M and returns a
  matrix of only the first row of M"
  (with-meta (take (:col_cnt (meta M)) M)
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) 0)
              :col_names (:col_names (meta M))}))

(defn first-col [M]
  "Accepts a matrix M and returns a
  matrix of only the first column of M"
  (with-meta (take-nth (:col_cnt (meta M)) M)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt 1
              :row_names (:row_names (meta M))
              :col_names (get (:col_names (meta M)) 0)}))

(defn last-row [M]
  "Accepts a matrix M and returns a
  matrix of only the last row of M"
  (with-meta (subvec M
                     (- (count M) (:col_cnt (meta M)))
                     (count M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) (dec (count (:row_names (meta M)))))
              :col_names (:col_names (meta M))}))

(defn last-col [M]
  "Accepts a matrix M and returns a
  matrix of only the last column of M"
  (with-meta (reverse (take-nth (:col_cnt (meta M)) (reverse M)))
                              {:row_cnt (:row_cnt (meta M))
                               :col_cnt 1
                               :row_names (:row_names (meta M))
                               :col_names (get (:col_names (meta M)) (dec (count (:col_names (meta M)))))}))

(defn nth-row [M i]
  "Accepts a matrix M and index i.
  Returns a matrix of only the ith row of M"
  (with-meta (subvec M
                     (* i (:col_cnt (meta M)))
                     (* (inc i) (:col_cnt (meta M))))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) i)
              :col_names (:col_names (meta M))}))

(defn nth-col [M j]
  "Accepts a matrix M and index j.
  Returns a matrix of only the ith column of M"
  (with-meta (vec (take-nth (:col_cnt (meta M)) (drop j M)))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt 1
              :row_names (:row_names (meta M))
              :col_names (get (:col_names (meta M)) j)}))

(defn all-rows [M]
  "Accepts a matrix M and returns a
  vector of matrcies. Each matrix in
  the vector is a row of the original matrix M"
  (loop [i 0 out []]
    (if (>= i (:row_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-row M i))))))

(defn all-cols [M]
  "Accepts a matrix M and returns a
  vector of matrcies. Each matrix in
  the vector is a column of the original matrix M"
  (loop [i 0 out []]
    (if (>= i (:col_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-col M i))))))

(defn row [M label]
  "Accepts a matrix M and row name label
  returns the index of the row with the corresponding label."
  (nth-row M (.indexOf (:row_names (meta M)) label)))

(defn col [M label]
  "Accepts a matrix M and column name label
  returns the index of the column with the corresponding label."
  (nth-col M (.indexOf (:col_names (meta M)) label)))

(defn max-ele-length [M]
  "Accepts a matrix M and returns the max
  string length of all values in the matrix."
  (let [rtnVal (inc (apply max (map count (map str M))))]
    (if (some nil? M)
      (max 5 rtnVal)  ;the format function replaces nil with null, which needs five charaters to format  
      rtnVal)))
 
(defn fmt-str [M & [justify _]]
  "Accepts a matrix M and optional
  :left or :right justify keyword.
  Returns the format string used
  in the format function for printing"
  (str "%"
       ({:left "-" :right ""} justify "-")
       (max-ele-length M)
       "s"))
 
(defn matrix-str [M & [justify _]]
  "Accepts a matrix M and optional
  :left or :right justify keyword.
  Returns a string representatin of 
  the matrix"
  (loop [row 0 rtnStr ""]
    (if (= row (:row_cnt (meta M)))
      (clojure.string/trim rtnStr)
      (recur (inc row) (str rtnStr
                            (apply str (map #(format (fmt-str M justify) %) (nth-row M row)))
                             "\n")))))

(defn reflect [M]
  "Accepts a matrix M and returns M reflected across its y-axis.
  (Think 'mirror reflection')"
  (with-meta (vec (apply concat (map reverse (partition (:col_cnt (meta M)) M))))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))

(defn flip [M]
  "Accepts a matrix M and returns M flipped across its x-axis.
  (Think 'flipped upside down')"
  (loop [i (dec (:row_cnt (meta M))) out []]
    (if (< i 0)
      (with-meta (vec out) {:row_cnt (:row_cnt (meta M))
                            :col_cnt (:col_cnt (meta M))
                            :row_names (:row_names (meta M))
                            :col_names (:col_names (meta M))})
      (recur (dec i)
             (concat out (nth-row M i))))))

(defn diagonal [M]
  "Accepts a matrix M and returns a matrix of values
  along the diagonal going from the top-left corner
  to bottom-right corner of M"
  (with-meta (vec (take-nth (+ 1 (:col_cnt (meta M))) M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names []
              :col_names (:col_names(meta M))}))

(defn anti-diagonal [M]
  "Accepts a matrix M and returns a matrix of values
  along the diagonal going from the top-right corner
  to bottom-left corner of M"
  (diagonal (reflect M)))

(defn transpose [M]
  "Accepts a matrix M and returns the transpose of M"
  (loop [i 0 out (matrix (:col_cnt (meta M)) (:row_cnt (meta M)))]
    (if (= i (* (:row_cnt (meta M)) (:col_cnt (meta M))))
      (with-meta out {:row_cnt (:col_cnt (meta M))
                      :col_cnt (:row_cnt (meta M))
                      :row_names (:col_names (meta M))
                      :col_names (:row_names (meta M))})
      (recur 
        (inc i)
        (matrix-assoc out
                      (get-pos out [(get-col M i) (get-row M i)])
                      (get M i))))))

(defn drop-nth-row [M i]
  "Accepts a matrix M and row index i.
  Returns a the original matrix with the ith row removed"
  (with-meta (vec (concat (subvec M 0 (* i (:col_cnt (meta M))))
                 (subvec M (* (inc i) (:col_cnt (meta M))))))
             {:row_cnt (dec (:row_cnt (meta M)))
              :col_cnt (:col_cnt (meta M))
              :row_names (vec-remove (:row_names (meta M)) 0)
              :col_names (:col_names (meta M))}))

(defn drop-nth-col [M j]
  "Accepts a matrix M and column index j.
  Returns a the original matrix with the jth column removed"
  (transpose (drop-nth-row (transpose M) j)))

(defn sub-matrix [M [i j]]
  "Accepts a matrix M and vector containing a row and column index.
  Returns a the original matrix with the ith row and jth column removed"
  (drop-nth-row (drop-nth-col M j) i))

(defn row-count [M]
  "Accepts a matrix M and returns the number of rows it has"
  (:row_cnt (meta M)))

(defn col-count [M]
  "Accepts a matrix M and returns the number of columns it has"
  (:col_cnt (meta M)))

(defn row-names [M]
  (:row_names (meta M)))

(defn col-names [M]
  (:col_names (meta M)))

(defn row-index [M label]
  "Accepts a matrix and keyword and returns
  the row number associated with that keyword"
  (.indexOf (row-names M) label))

(defn col-index [M label]
  "Accepts a matrix and keyword and returns
  the column number associated with that keyword"
  (.indexOf (col-names M) label))

(defn get-ele [M pos]
  (nth M pos))

(defn get-cell [M [i j]]
  (nth M (get-pos M [i j])))

(defn csv-str [M]
  "Accepts a matrix and create a csv string of it."
  (apply str
    (apply concat
           (interpose "," (cons :row_names ((meta M) :col_names))) ;append columns as first row of csv
           "\n" ;line break aft ther column names
           (interpose "\n" ;put a line break after each row
                      (map #(interpose "," %) ; interpose commas for csv format
                           (map #(apply cons %) (partition 2 (interleave ((meta M) :row_names) ;append the row name to the beginning or each row
                                                                         (all-rows M)))))))))

(defn csv-spit [file-path M]
  "Accepts a file path and a matrix,
  and writes the matrix to the file in csv format."
  (spit file-path (csv-str M)))

(defn csv-slurp [file-path]
  "Accepts a csv file as a file path (string) and creates a matrix"
  (let [file (map #(clojure.string/split % #",")
                  (clojure.string/split-lines (slurp file-path)))]
    (drop-nth-col (with-meta (vec (apply concat (rest file)))
                             {:row_names (vec (map #(keyword (first %)) (rest file)))
                              :col_names (vec (map keyword (first file)))
                              :row_cnt (count row-names)
                              :col_cnt (count col-names)})
                  0)))