(ns com.github.stepbeekio.sudoku-solver-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.github.stepbeekio.sudoku-solver :as sut]))

(deftest empty-grid-test
  (let [grid (sut/empty-grid)]
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
  (testing "every cell has 20 peers, plus itself"
    (is (= 21 (count (filter #(sut/related? [5 5] %)
                             (keys (sut/empty-grid))))))))

(deftest set-cell-test
  (let [grid (sut/set-cell (sut/empty-grid) [1 1] 4)]
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
  (testing "setting a cell that is already set throws"
    (let [grid (assoc (sut/empty-grid) [1 1] #{4})]
      (is (thrown? clojure.lang.ExceptionInfo
                   (sut/set-cell grid [1 1] 5))))))
