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
  (cond
    (= (get grid coord) #{digit}) grid
    (= 1 (count (get grid coord))) (ex-info "Attempting to set already set grid entry" {:grid grid :coord coord :existing (get grid coord)})
    :else (reduce-kv (fn [m k v]
                       (cond
                         (related? coord k) (assoc m k (set (remove #(= digit %) v)))
                         (= coord k) (assoc m k #{digit})
                         :else (assoc m k v)))
                     {}
                     grid)))

(defn solved? [grid]
  (every? #(= 1 (count (val %))) grid))

(comment (solved? {[1 1] #{3} [2 2] #{4}}))

(defn possible-moves
  "Takes in the grid as a map of [x y] to set of possible digits and returns a vector of vector choices [[[x y] a] [[x y] b]]"
  [grid]
  (if (every? #(>= (count (val %)) 1) grid)
    (for [[k v] grid  d v :when (> (count v) 1)] [k d])
    []))

(defn solve [grid]
  (if (solved? grid)
    grid
    (some (fn [[c d]] (solve (set-cell grid c d)))
          (possible-moves grid))))

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
    (tap> {:coordinates coordinates :grid-with-zeroes grid-with-zeroes :grid-with-possibilities grid-with-possibilities})
    (reduce-kv (fn [m k v] (if (= 1 (count v))
                             (set-cell m k (first v))
                             m)) grid-with-possibilities grid-with-possibilities)))
(+ 1 2)
(comment (sort-by key  (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000")))
(def example (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000"))

(comment (solve example))

