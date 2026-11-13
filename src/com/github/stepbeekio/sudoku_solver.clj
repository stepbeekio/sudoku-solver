(ns com.github.stepbeekio.sudoku-solver)

(defn empty-grid []
  (into (sorted-map)  (for [x (range 1 10) y (range 1 10)] {[x y] (into #{} (range 1 10))})))

(defn block [coord]
  (map #(quot (- % 1) 3) coord))

(defn related?
  "Takes in two coordinates a and b and returns true if they impact each other. Returns false if equal"
  [a b]
  (and (not= a b)
       (or (= (first a) (first b))
           (= (second a) (second b))
           (= (block a) (block b)))))

(defn set-cell
  "Sets the cell digit on a particular coordinate. Also removes that digit from any related cells that still allow it"
  [grid coord digit]
  (let [move-made (assoc grid coord #{digit})]
    (reduce-kv (fn [m k _]
                 (cond
                   (related? coord k) (let [current (get m k)
                                            updated (set (remove #(= digit %) current))
                                            decided (= 1 (count updated))
                                            changed (not= current updated)]
                                        (cond (empty? updated) (throw (ex-info "Results in an empty choice set" {:grid grid :coord coord  :digit digit :other {:coord k :existing current}}))
                                              (and changed decided) (set-cell m k (first updated))
                                              :else (assoc m k updated)))
                   :else m))
               move-made
               move-made)))

(defn solved? [grid]
  (every? #(= 1 (count (val %))) grid))

(comment (solved? {[1 1] #{3} [2 2] #{4}}))

(defn possible-moves
  "Takes in the grid as a map of [x y] to set of possible digits and returns a vector of vector choices [[[x y] a] [[x y] b]]"
  [grid]
  (let [filtered (filter (fn [[_ v]] (> (count v) 1)) grid)
        sorted (sort-by (fn [[_ v]] (count v)) filtered)]
    (for [[k v] sorted d v] [k d])))

(defn solve
  ([grid] (solve grid 5))
  ([grid depth-limit]
   (cond
     (= depth-limit 0) grid
     (solved? grid) grid
     :else (some (fn [[c d]] (solve (set-cell grid c d) (- depth-limit 1)))
                 (possible-moves grid)))))

(comment (possible-moves {[1 1] #{1 2 3 4 5} [3 3] #{4 5 6}}))
(comment (possible-moves {[1 1] #{} [3 3] #{4 5 6}}))

(defn parse-sudoku [s]
  (let [coordinates (for [x (range 1 10) y (range 1 10)]
                      [x y])
        grid-with-zeroes (zipmap coordinates (map #(Character/digit (char %) 10) s))
        grid-with-possibilities (reduce-kv (fn [m k v]
                                             (if (= v 0)
                                               (assoc m k (set (range 1 10)))
                                               (assoc m k #{v}))) {} grid-with-zeroes)]
    (reduce-kv (fn [m k v] (if (= 1 (count v))
                             (set-cell m k (first v))
                             m)) grid-with-possibilities grid-with-possibilities)))

;; 000260701
;; 680070090
;; 190004500
;; 820100040
;; 004602900
;; 050003028
;; 009300074
;; 040050036
;; 703018000

(comment (sort-by key  (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000")))
(def example (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000"))

(comment (solve example))

