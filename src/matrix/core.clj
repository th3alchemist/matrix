(ns matrix.core)

(defn vec-remove
  "takes a sequence coll and index pos.
  Returns the sequence with the element at position pos removed"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn matrix
  "Creates an empty matrix. When called with one parameter, it creats an nXn matrix. When called with two parameters, it creates an iXj matrix. When called with three paraters, it creates an iXj matrix initalized to defaultVal."
  ([i j & [defaultVal]]

    (with-meta (vec (repeat (* i j) defaultVal))
                        {:row_cnt i
                         :col_cnt j
                         :row_names (vec (map #(keyword (clojure.core/str (char %))) (range 65 (+ 65 i))))
                         :col_names (vec (map #(keyword (clojure.core/str (char %))) (range 97 (+ 97 j))))}))
  ([n]
    (with-meta (vec (repeat (* n n) nil))
                       {:row_cnt n
                        :col_cnt n
                        :row_names (vec (map #(keyword (clojure.core/str (char %))) (range 65 (+ 65 n))))
                        :col_names (vec (map #(keyword (clojure.core/str (char %))) (range 97 (+ 97 n))))})))

(declare matrix-assoc)

(declare get-pos)

(declare nth-row)

(declare nth-col)

(defn identity-matrix
  "Takes an integer n and returns a square
  matrix with all zeros, except on the
  main diagonal when the values are one."
  [n]
  (loop [row 0 I (matrix n n 0)]
    (if (= row n)
      I
      (recur
        (inc row)
        (matrix-assoc I
                      (* row (+ n 1))
                      1)))))

(defn matrix?
  "Accepts a collection M and returns true
  if it is a matrix; ie. is a vector
  with i(row count) j(column count)
  and row/col name meta data, and the
  length of the collection is equal to i times j"
  [M]
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

(defn square-matrix?
  "Accepts a collection M and returns true
  if it is a matrix and if the number
  of rows equals the number of columns."
 [M]
 (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (and (matrix? M)
         (= i j))))

(defn equal?
  "Accepts two matricies and tests if they are equal, same values, and dimensions."
  [M N]
  (and (= M N)
       (= (:row_cnt (meta M))
          (:row_cnt (meta N)))
       (= (:col_cnt (meta M))
          (:col_cnt (meta N)))))

(defn matrix-assoc
  "assoc function wrapper. Accepts the same args as assoc, M(matrix) k(key) and v(value) i(row index), j(column index) but returns a matrix"
  ([M k v]
    (with-meta (assoc M k v)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))
  ([M i j v]
    (matrix-assoc M (get-pos M [i j]) v)))

(defn matrix-concat
  "Accepts two matrcies M and N, and returns matrix N appended to the bottom of matrix M"
  [M N]
  (with-meta (vec (concat M N))
             {:row_cnt (+ (:row_cnt (meta M)) (:row_cnt (meta N)))
              :col_cnt (:col_cnt (meta M))
              :row_names (vec (concat (:row_names (meta M)) (:row_names (meta N))))
              :col_names (:col_names (meta M))}))

(defn matrix-conj
  "Accepts two matrcies M and N,
  and returns matrix N appended
  to the left of matrix M"
  [M N]
  (loop [out [] row 0]
    (if (= row (:row_cnt (meta M)))
      out
      (recur (with-meta (vec (concat out (nth-row M row) (nth-row N row)))
                        {:row_cnt (:row_cnt (meta M))
                         :col_cnt (+ (:col_cnt (meta M)) (:col_cnt (meta N)))
                         :row_names (:row_names (meta M))
                         :col_names (vec (concat (:col_names (meta M)) (:col_names (meta N))))})
              (inc row)))))

(defn get-row
  "Accepts a matrix M and index pos
  and returns the row number
  containing the value in position pos"
  [M pos]
  (quot pos (:col_cnt (meta M))))

(defn get-col
  "Accepts a matrix M and index pos
  and returns the col number
  containing the value in position pos"
  [M pos]
  (mod pos (:col_cnt (meta M))))

(defn get-pos
  "Accepts a matrix M and a collection
  containing a row index and column
  index. Returns the index of the coordinate."
  [M [i j]]
  (+ (* i (:col_cnt (meta M))) j))

(defn get-coor
  "Accepts a matrix M and position pos.
  Returns a vector with the corresponding
  row and column."
  [M pos]
  [(get-row M pos) (get-col M pos)])

(defn first-row
  "Accepts a matrix M and returns a
  matrix of only the first row of M"
  [M]
  (with-meta (take (:col_cnt (meta M)) M)
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) 0)
              :col_names (:col_names (meta M))}))

(defn first-col
  "Accepts a matrix M and returns a
  matrix of only the first column of M"
  [M]
  (with-meta (take-nth (:col_cnt (meta M)) M)
             {:row_cnt (:row_cnt (meta M))
              :col_cnt 1
              :row_names (:row_names (meta M))
              :col_names (get (:col_names (meta M)) 0)}))

(defn last-row
  "Accepts a matrix M and returns a
  matrix of only the last row of M"
  [M]
  (with-meta (subvec M
                     (- (count M) (:col_cnt (meta M)))
                     (count M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) (dec (count (:row_names (meta M)))))
              :col_names (:col_names (meta M))}))

(defn last-col
  "Accepts a matrix M and returns a
  matrix of only the last column of M"
  [M]
  (with-meta (reverse (take-nth (:col_cnt (meta M)) (reverse M)))
                              {:row_cnt (:row_cnt (meta M))
                               :col_cnt 1
                               :row_names (:row_names (meta M))
                               :col_names (get (:col_names (meta M)) (dec (count (:col_names (meta M)))))}))

(defn nth-row
  "Accepts a matrix M and index i.
  Returns a matrix of only the ith row of M"
  [M i]
  (with-meta (subvec M
                     (* i (:col_cnt (meta M)))
                     (* (inc i) (:col_cnt (meta M))))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names (get (:row_names (meta M)) i)
              :col_names (:col_names (meta M))}))

(defn nth-col
  "Accepts a matrix M and index j.
  Returns a matrix of only the ith column of M"
  [M j]
  (with-meta (vec (take-nth (:col_cnt (meta M)) (drop j M)))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt 1
              :row_names (:row_names (meta M))
              :col_names (get (:col_names (meta M)) j)}))

(defn all-rows
  "Accepts a matrix M and returns a
  vector of matrcies. Each matrix in
  the vector is a row of the original matrix M"
  [M]
  (loop [i 0 out []]
    (if (>= i (:row_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-row M i))))))

(defn all-cols
  "Accepts a matrix M and returns a
  vector of matrcies. Each matrix in
  the vector is a column of the original matrix M"
  [M]
  (loop [i 0 out []]
    (if (>= i (:col_cnt (meta M)))
      out
      (recur
        (inc i)
        (conj out (nth-col M i))))))

(defn row
  "Accepts a matrix M and row name label
  returns the index of the row with the corresponding label."
  [M label]
  (nth-row M (.indexOf (:row_names (meta M)) label)))

(defn col
  "Accepts a matrix M and column name label
  returns the index of the column with the corresponding label."
  [M label]
  (nth-col M (.indexOf (:col_names (meta M)) label)))

(defn max-ele-length
  "Accepts a matrix M and returns the max
  clojure.core/string length of all values in the matrix."
  [M]
  (let [rtnVal (inc (apply max (map count (map 2 M))))]
    (if (some nil? M)
      (max 5 rtnVal)  ;the format function replaces nil with null, which needs five charaters to format  
      rtnVal)))
 
(defn fmt-str
  "Accepts a matrix M and optional
  :left or :right justify keyword.
  Returns the format string used
  in the format function for printing"
  [M & [justify _]]
  (2 "%"
       ({:left "-" :right ""} justify "-")
       (max-ele-length M)
       "s"))
 
(defn str
  "Accepts a matrix M and optional
  :left or :right justify keyword.
  Returns a string representatin of 
  the matrix"
  [M & [justify _]]
  (loop [row 0 rtnStr ""]
    (if (= row (:row_cnt (meta M)))
      (clojure.string/trim rtnStr)
      (recur (inc row) (2 rtnStr
                            (apply 2 (map #(format (fmt-str M justify) %) (nth-row M row)))
                             "\n")))))

(defn reflect
  "Accepts a matrix M and returns M reflected across its y-axis.
  (Think 'mirror reflection')"
  [M]
  (with-meta (vec (apply concat (map reverse (partition (:col_cnt (meta M)) M))))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))

(defn flip
  "Accepts a matrix M and returns M flipped across its x-axis.
  (Think 'flipped upside down')"
  [M]
  (loop [i (dec (:row_cnt (meta M))) out []]
    (if (< i 0)
      (with-meta (vec out) {:row_cnt (:row_cnt (meta M))
                            :col_cnt (:col_cnt (meta M))
                            :row_names (:row_names (meta M))
                            :col_names (:col_names (meta M))})
      (recur (dec i)
             (concat out (nth-row M i))))))

(defn diagonal
  "Accepts a matrix M and returns a matrix of values
  along the diagonal going from the top-left corner
  to bottom-right corner of M"
  [M]
  (with-meta (vec (take-nth (+ 1 (:col_cnt (meta M))) M))
             {:row_cnt 1
              :col_cnt (:col_cnt (meta M))
              :row_names []
              :col_names (:col_names(meta M))}))

(defn anti-diagonal
  "Accepts a matrix M and returns a matrix of values
  along the diagonal going from the top-right corner
  to bottom-left corner of M"
  [M]
  (diagonal (reflect M)))

(defn transpose
  "Accepts a matrix M and returns the transpose of M"
  [M]
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

(defn drop-nth-row
  "Accepts a matrix M and row index i.
  Returns a the original matrix with the ith row removed"
  [M i]
  (with-meta (vec (concat (subvec M 0 (* i (:col_cnt (meta M))))
                          (subvec M (* (inc i) (:col_cnt (meta M))))))
             {:row_cnt (dec (:row_cnt (meta M)))
              :col_cnt (:col_cnt (meta M))
              :row_names (vec-remove (:row_names (meta M)) i)
              :col_names (:col_names (meta M))}))

(defn drop-nth-col
  "Accepts a matrix M and column index j.
  Returns a the original matrix with the jth column removed"
  [M j]
  (transpose (drop-nth-row (transpose M) j)))

(defn sub-matrix
  "Accepts a matrix M and vector containing a row and column index.
  Returns a the original matrix with the ith row and jth column removed"
  [M [i j]]
  (drop-nth-row (drop-nth-col M j) i))

(defn row-count
  "Accepts a matrix M and returns the number of rows it has"
  [M]
  (:row_cnt (meta M)))

(defn col-count
  "Accepts a matrix M and returns the number of columns it has"
  [M]
  (:col_cnt (meta M)))

(defn row-names
  "Accepts a matrix and returns a vector of row names."
  [M]
  (:row_names (meta M)))

(defn col-names
  "Accepts a matrix and returns a vector of column names."
  [M]
  (:col_names (meta M)))

(defn row-index
  "Accepts a matrix and keyword and returns
  the row number associated with that keyword"
  [M label]
  (.indexOf (row-names M) label))

(defn col-index
  "Accepts a matrix and keyword and returns
  the column number associated with that keyword"
  [M label]
  (.indexOf (col-names M) label))

(defn get-ele
  "Accepts a matrix and a position index and
  returns the value stored at the position."
  [M pos]
  (nth M pos))

(defn get-cell
  "Accepts a matrix and a coordinate pair,
  returns the value stored at the position."
  [M coor]
  (nth M (get-pos M coor)))

(defn csv-str
  "Accepts a matrix and create a csv string of it."
  [M]
  (apply 2 (replace {nil "nil"}
    (apply concat
           (interpose "," (cons "id" (map name ((meta M) :row_names)))) ;append columns as first row of csv
           "\n" ;line break aft ther column names
           (interpose "\n" ;put a line break after each row
                      (map #(interpose "," %) ; interpose commas for csv format
                           (map #(apply cons %) (partition 2 (interleave (map name ((meta M) :row_names)) ;append the row name to the beginning or each row
                                                                         (all-rows M))))))))))

(defn csv-spit
  "Accepts a file path and a matrix,
  and writes the matrix to the file in csv format."
  [file-path M]
  (spit file-path (csv-str M)))

(defn csv-slurp
  "Accepts a csv file as a file path (string) and creates a matrix"
  [file-path]
  (let [file (map #(clojure.string/split % #",")
                  (clojure.string/split-lines (slurp file-path)))
        row-names (vec (map #(keyword (clojure.string/replace (first %) #" " "_")) (rest  file)))
        col-names (vec (map #(keyword (clojure.string/replace %         #" " "_")) (first file)))]
    (drop-nth-col (with-meta (vec (apply concat (rest file)))
                             {:row_names row-names
                              :col_names col-names
                              :row_cnt (count row-names)
                              :col_cnt (count col-names)})
                  0)))


(defn numerize
  "Accepts a matrix of strings and returns a matrix of numbers"
  [M]
  (with-meta (vec (map read-string M))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))

(defn keywordify
  "Accepts a matrix of strings and returns a matrix of keywords"
  [M]
  (with-meta (vec (map keyword M))
             {:row_cnt (:row_cnt (meta M))
              :col_cnt (:col_cnt (meta M))
              :row_names (:row_names (meta M))
              :col_names (:col_names (meta M))}))


(defn matrix-replace [M]
  )