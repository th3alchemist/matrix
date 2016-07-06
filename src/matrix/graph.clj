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
  "returns a hash-map of nodes reachable from the passed node, and their weights"
  [G src]
  (into {}
        (filter (comp some? val)
                (apply hash-map
                       (interleave
                         (:row_names (meta G))
                         (mCore/row G src))))))

(defn bfs
  "breadth-first serach function for a*"
  [coll]
  (if (empty? coll)
    nil
    (key (first coll))))

(defn dfs
  "depth-first serach function for a*"
  [coll]
  (if (empty? coll)
    nil
    (key (last coll))))

(defn pfs
  "priority-first serach function for a*, the vertex with the lowest weighted is given priority"
  [coll]
  (if (empty? coll)
    nil
    (key (first (into (sorted-map-by (fn [key1 key2]
                       (compare (get coll key1)
                                (get coll key2))))
      coll)))))

;dijkstra - use pfs, but start with a processed list of all inf, update values in processed and choose the lowest, added a closed vector

(defn a*
  "returns a vector of vertices from the src to dest in function f serach order"
  [G f src & [dest]]
  (loop [open (get-children G src) processed [src]]
    (if (= (f open) dest)
      (conj processed dest)
      (recur
        (apply dissoc
               (merge open
                      (get-children G (f open)))
               (conj processed (f open)))
        (conj processed (f open))))))

;(a* weighted-graph dijkstra :a)