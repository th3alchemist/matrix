(ns matrix.util)


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


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