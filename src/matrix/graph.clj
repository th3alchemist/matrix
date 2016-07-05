(ns matrix.graph)

(require 'matrix.core)
(alias 'mCore 'matrix.core)

(defn child-name 
  "accepts a matrix and index. Returns the column name assigned to the index pos"
  [row i]
  (if (nth row i)
      (nth (:col_names (meta row)) i)
      nil))

(defn vertex-name
  "accepts a graph and vertex in the graph. Returns the column name assigned to the vertex pos"
  [G vertex]
  (nth (:col_names (meta G)) vertex))

(defn get-children
  "returns a list of nodes reachable from the passed row"
  [row]
  (loop [i 0 lst []]
    (if (>= i (:col_cnt (meta row)))
      (remove nil? lst)
      (recur (inc i) (conj lst (child-name row i))))))

(defn get-children-vec
  "returns a vector of nodes reachable from the passed row"
  [row]
  (vec (reverse (get-childrne row))))

(defn get-distance [G src dest]
  (mCore/get-cell G [(mCore/row-index G src)
                     (mCore/col-index G dest)]))

(defn dfs 
  "returns a vector of vertices from the src to dest in breath-first serach order"
  [G src & [dest]]
  (loop [open [src] processed []] ;;use as stack
    (println "open is " open)
    (if (= (peek open) dest)
      (conj processed dest)
      (recur
        (vec
          (remove (conj (set processed)
                       (peek open))
                  (concat open
                          (get-children-vec (mCore/row G (peek open))))))
        (conj processed (peek open))))))

(defn bfs
  "returns a vector of vertices from the src to dest in depth-first serach order"
  [G src & [dest]]
  (loop [open (list src) processed []] ;;use as queue
    (if (= (peek open) dest)
      (conj processed dest)
      (recur
        (apply list
               (remove (conj (set processed)
                             (peek open))
                       (concat open
                               (get-children (mCore/row G (peek open))))))
        (conj processed (peek open))))))

;dijkstra - get children with ditances, then sort by values
;(into (sorted-map-by (fn [key1 key2]
;                       (compare (get {:a 19 :b 8 :c 5 :d 17} key2)
;                                (get {:a 19 :b 8 :c 5 :d 17} key1))))
;      {:a 19 :b 8 :c 5 :d 17})

(defn a*
  "returns a vector of vertices from the src to dest in function f serach order"
  [G f src & [dest]]
  (loop [open (get-kids G src) processed [src]]
    (if (= (f open) dest)
      (conj processed dest)
      (recur
        (apply dissoc
               (merge open
                      (get-kids G (f open)))
               (conj processed (f open)))
        (conj processed (f open))))))

(defn get-kids
  "returns a hash-map of nodes reachable from the passed node, and their weights"
  [G src]
  (into {}
        (filter (comp some? val)
                (apply hash-map
                       (interleave
                         (:row_names (meta G))
                         (mCore/row G src))))))


(defn prioritize-queue [coll]
  "accepts map of node:dist"
  (key (first (into (sorted-map-by (fn [key1 key2]
                       (compare (get coll key1)
                                (get coll key2))))
      coll))))

(b* bint bfs :a)

(defn bfs [coll]
  (if (empty? coll)
    nil
    (key (first coll))))

(defn dfs [coll]
  (if (empty? coll)
    nil
    (key (last coll))))