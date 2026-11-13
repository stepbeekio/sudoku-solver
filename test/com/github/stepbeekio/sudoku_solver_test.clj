(ns com.github.stepbeekio.sudoku-solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.github.stepbeekio.sudoku-solver :as sut]))

(defn blank-grid
  "A grid with no givens: every cell has all nine candidates."
  []
  (sut/parse-sudoku (apply str (repeat 81 \0))))

(deftest parse-blank-puzzle-test
  (let [grid (blank-grid)]
    (testing "has a cell for each of the 81 coordinates"
      (is (= 81 (count grid)))
      (is (contains? grid [1 1]))
      (is (contains? grid [9 9])))
    (testing "every cell starts with all nine candidates"
      (is (every? #(= #{1 2 3 4 5 6 7 8 9} %) (vals grid))))))

(deftest block-test
  (testing "coordinates on either side of a box boundary land in different boxes"
    (is (= [0 0] (sut/block [3 3])))
    (is (= [1 1] (sut/block [4 4]))))
  (testing "rows and columns map to boxes independently"
    (is (= [0 1] (sut/block [1 6])))
    (is (= [2 0] (sut/block [7 3]))))
  (testing "the last coordinate is in the last box"
    (is (= [2 2] (sut/block [9 9])))))

(deftest related?-test
  (testing "cells sharing a row, column or box are related"
    (is (sut/related? [1 1] [1 9]))
    (is (sut/related? [1 1] [9 1]))
    (is (sut/related? [1 1] [3 3])))
  (testing "cells sharing none of those are not related"
    (is (not (sut/related? [1 1] [4 4])))
    (is (not (sut/related? [3 3] [4 4]))))
  (testing "a cell is not related to itself"
    (is (not (sut/related? [5 5] [5 5]))))
  (testing "every cell has 20 peers"
    (is (= 20 (count (filter #(sut/related? [5 5] %)
                             (keys (blank-grid))))))))

(deftest set-cell-test
  (let [grid (sut/set-cell (blank-grid) [1 1] 4)]
    (testing "the target cell is pinned to the digit"
      (is (= #{4} (get grid [1 1]))))
    (testing "the digit is removed from peers in the same row, column and box"
      (is (= #{1 2 3 5 6 7 8 9} (set (get grid [1 9]))))
      (is (= #{1 2 3 5 6 7 8 9} (set (get grid [9 1]))))
      (is (= #{1 2 3 5 6 7 8 9} (set (get grid [3 3])))))
    (testing "unrelated cells keep all their candidates"
      (is (= #{1 2 3 4 5 6 7 8 9} (get grid [5 5]))))
    (testing "peers' candidates are still sets"
      (is (set? (get grid [1 9]))))
    (testing "all 81 cells are kept"
      (is (= 81 (count grid)))))
  (testing "returns nil when a peer would be left with no candidates"
    (let [grid (assoc (blank-grid) [1 9] #{5})]
      (is (nil? (sut/set-cell grid [1 1] 5))))))

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

(deftest solve-examples-test
  (doseq [{:keys [name puzzle solution]} examples]
    (testing name
      (is (= solution (sut/format-solution (sut/solve (sut/parse-sudoku puzzle))))))))
