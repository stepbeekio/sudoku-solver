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
                                        (cond
                                          (empty? updated) (reduced nil)
                                          (and changed decided) (set-cell m k (first updated))
                                          :else (assoc m k updated)))
                   :else m))
               move-made
               move-made)))

(defn solved? [grid]
  (every? #(= 1 (count (val %))) grid))

(comment (solved? {[1 1] #{3} [2 2] #{4}}))

(defn possible-moves
  "Takes in the grid as a map of [x y] to set of possible digits "
  [grid]
  (let [filtered (filter (fn [[_ v]] (> (count v) 1)) grid)
        sorted (sort-by (fn [[_ v]] (count v)) filtered)
        [c options] (first sorted)]
    (for [d options] [c d])
    )
  )

(defn solve
  [grid]
   (cond
     (nil? grid) nil
     (solved? grid) grid
     :else (some (fn [[c d]] (solve (set-cell grid c d) ))
                 (possible-moves grid))))

(comment (possible-moves {[1 1] #{1 2 3 4 5} [2 3] #{4 6} [3 3] #{4 5 6}}))
(def t [[3 3] #{4 6 5}])
(for [[a b] t] [a b])  
; 
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

(comment (sort-by key  (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000")))
(def example (parse-sudoku "000260701680070090190004500820100040004602900050003028009300074040050036703018000"))

(comment (solve example))

(defn format-solution [grid]
  (if (nil? grid) nil
    
  (let [sorted (sort-by key grid)
        values (map (fn [[_ v]] (first v)) sorted)]
    (apply str values))))

;; Test puzzles: 81-char row-major strings, 0 = blank. Each has a unique solution.
;; Difficulty is relative: :easy falls to naked singles alone; the rest need search.
(def examples
  [{:name       "easy (original example)"
    :difficulty :easy
    :puzzle     "000260701680070090190004500820100040004602900050003028009300074040050036703018000"
    :solution   "435269781682571493197834562826195347374682915951743628519326874248957136763418259"}
   {:name       "easy (Project Euler #96, grid 1)"
    :difficulty :easy
    :puzzle     "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
    :solution   "483921657967345821251876493548132976729564138136798245372689514814253769695417382"}
   {:name       "medium"
    :difficulty :medium
    :puzzle     "200080300060070084030500209000105408000000000402706000301007040720040060004010003"
    :solution   "245981376169273584837564219976125438513498627482736951391657842728349165654812793"}
   {:name       "hard"
    :difficulty :hard
    :puzzle     "000000907000420180000705026100904000050000040000507009920108000034059000507000000"
    :solution   "462831957795426183381795426173984265659312748248567319926178534834259671517643892"}
   {:name       "very hard (21 clues)"
    :difficulty :very-hard
    :puzzle     "000000012000000003002300400001800005060070800000009000008500000900040500470006000"
    :solution   "839465712146782953752391486391824675564173829287659341628537194913248567475916238"}])

(comment (for [{:keys [name puzzle solution]} examples] {:name name :result (= (format-solution (solve (parse-sudoku puzzle))) solution)}))

