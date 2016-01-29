(ns matrix.util)


(defn foo
  "I don't do a whole lot."
  [x]
  (with-meta x {:row_cnt (:row_cnt (meta x))
                :col_cnt (:col_cnt (meta x))
                :col_names (:col_names (meta x))
                :row_names (:row_names (meta x))}))


(defn get-row [pos M] (let [col_cnt (:col_cnt (meta M))]
                        (quot pos col_cnt)))

(defn get-col [pos M] (let [col_cnt (:col_cnt (meta M))]
                        (mod pos col_cnt)))

(defn get-pos [i j M] (let [col_cnt (:col_cnt (meta M))] 
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


(defn scalar-add [scalar M]   (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map + M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar addition

(defn scalar-sub [scalar M] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
    (with-meta (map - M (repeat (* i j) scalar)){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))})));;scalar subtraction
  
  
(defn scalar-mul [scalar M] (let [i (:row_cnt (meta M)) j (:col_cnt (meta M))]
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

(defn matrix-mul [M N] (if true
                         (do (with-meta (map * M N){
               :row_cnt (:row_cnt (meta M))
               :col_cnt (:col_cnt (meta M))
               :col_names (:col_names (meta M))
               :row_names (:row_names (meta M))}))))

;;(apply + (map * '(9 8) '(7 6)))











	


