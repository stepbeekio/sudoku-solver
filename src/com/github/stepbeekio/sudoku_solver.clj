(ns com.github.stepbeekio.sudoku-solver
  "Depth-first search based sudoku solver for learning some clojure")

(def ^:private all-digits
  (range 1 10))

(defn block
  "Normalizes a coordinate to the root of a block. E.g [1 3] becomes [1 1]."
  [coord]
  (map #(quot (dec %) 3) coord))

(defn related?
  "Memoized function that determines whether two coordinates in a sudoku game are related. Relations are on the same row, column or in the same 3x3 square. A cell is not related to itself."
  [a b]
  (and (not= a b)
       (or (= (first a) (first b))
           (= (second a) (second b))
           (= (block a) (block b)))))

(def ^:private coords
  (for [a all-digits b all-digits] [a b]))

(def coord->peers
  "Map of coordinates to a vector of peers. Precomputed to speed up the lookups."
  (into {} (for [c coords]
             [c (filterv #(related? c %) coords)])))

(defn- decided?
  "A cell is decided when there's only one possible candidate solution"
  [candidates]
  (= 1 (count candidates)))

(defn set-cell
  "Sets the cell digit on a particular coordinate. Also removes that digit from any related cells that still allow it"
  [grid coord digit]
  (let [updated-grid (assoc grid coord #{digit})
        related-coords (coord->peers coord)]
    (reduce (fn [m k]
              (let [current (get m k)
                    updated (disj current digit)]
                (cond
                  (empty? updated) (reduced nil)
                  (and (not= current updated) (decided? updated)) (or  (set-cell m k (first updated)) (reduced nil))
                  :else (assoc m k updated)))) updated-grid related-coords)))

(defn solved? [grid]
  (every? #(decided? %) (vals grid)))

(defn count-related-updates
  "Counts how many related cells will be changed as a result of this cell being set to this digit."
  [grid coord digit]
  (let [relations (coord->peers coord)
        requiring-update (filter (fn [k] (contains? (get grid k) digit)) relations)]
    (count requiring-update)))

(defn possible-moves
  "Takes in the grid as a map of [x y] to set of possible digits and returns the moves
   [[[x y] d] ...] for the single most-constrained undecided cell. Empty if all cells are decided."
  [grid]
  (let [[c options] (->> grid
                         (remove (comp decided? val))
                         (sort-by (comp count val))
                         first)]
    (->> options
         (sort-by #(count-related-updates grid c %))
         (map #(vector c %)))))

(defn solve
  "Returns a solved grid, or nil if there is no solution from here."
  [grid]
  (when grid
    (if (solved? grid) grid
        (some (fn [[c d]] (solve (set-cell grid c d)))
              (possible-moves grid)))))

(defn parse-sudoku
  "Parses in a flat string what represents a sudoku game. Blank square are represented by 0"
  [s]
  (let [grid-with-zeroes (zipmap coords (map #(Character/digit (char %) 10) s))
        grid-with-possibilities (update-vals grid-with-zeroes (fn [d] (if (zero? d) (set all-digits) #{d})))]
    (reduce-kv (fn [m k v] (if (decided? v)
                             (or  (set-cell m k (first v)) (reduced nil))
                             m)) grid-with-possibilities grid-with-possibilities)))

(defn format-solution
  "Formates a completed solution back into the flat string format used in parsing above."
  [grid]
  (when grid
    (->> grid
         (sort-by key)
         (map (fn [[_ v]] (first v)))
         (apply str))))

(comment (solved? {[1 1] #{3} [2 2] #{4}}))
(comment (possible-moves {[1 1] #{1 2 3 4 5} [2 3] #{4 6} [3 3] #{4 5 6}}))
(comment (possible-moves {[1 1] #{} [3 3] #{4 5 6}}))
(comment (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000"))
(comment (sort-by key  (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000")))
(comment (solve (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000")))

