(ns interpolation.core-test
  (:require [clojure.test :refer :all]
            [interpolation.core :refer :all]))

(deftest test-linear-interp
  (testing "Линейная интерполяция между двумя точками"
    (is (= 0.5 (linear-interp [0 0] [1 1] 0.5)))
    (is (= 1.5 (linear-interp [1 1] [2 2] 1.5)))
    (is (= 0.0 (linear-interp [0 0] [1 1] 0.0)))
    (is (= 1.0 (linear-interp [0 0] [1 1] 1.0)))))

(deftest test-newton-interp
  (testing "Интерполяция Ньютона для набора точек"
    (let [points [[0 0] [1 1] [2 4] [3 9]]]
      ;; Проверяем, что интерполяция возвращает значения для известных точек
      (is (= 0.0 (newton-interp points 0.0)))
      (is (= 1.0 (newton-interp points 1.0)))
      ;; Проверяем, что функция возвращает число
      (let [result (newton-interp points 1.5)]
        (is (number? result))
        (is (not (Double/isNaN result)))))))

(deftest test-parse-line
  (testing "Парсинг строк с координатами"
    (is (= [0.0 0.0] (parse-line "0 0")))
    (is (= [1.0 2.0] (parse-line "1 2")))
    (is (= [1.5 2.5] (parse-line "1.5 2.5")))
    (is (= [1.0 2.0] (parse-line "1;2")))
    (is (= [1.0 2.0] (parse-line "1\t2")))
    (is (nil? (parse-line "invalid")))))

(deftest test-is-sorted?
  (testing "Проверка сортировки точек"
    (is (true? (is-sorted? [] [1 1])))
    (is (true? (is-sorted? [[0 0]] [1 1])))
    (is (true? (is-sorted? [[0 0]] [0 1])))
    (is (false? (is-sorted? [[1 1]] [0 0])))))

(deftest test-gen-xs
  (testing "Генерация последовательности точек x"
    (is (= [0 1.0 2.0] (gen-xs 0 2 1.0 nil)))
    (is (= [1.5 2.0] (gen-xs 0 2 0.5 1.0)))
    (is (nil? (gen-xs 2 1 1.0 nil)))))

(deftest test-linear-algorithm
  (testing "Алгоритм линейной интерполяции"
    (let [algorithm (create-linear-algorithm)]
      (is (= 2 ((:window-size algorithm))))
      (is (= "linear" ((:algorithm-name algorithm))))
      (is (true? ((:can-process? algorithm) [[0 0] [1 1]])))
      (is (false? ((:can-process? algorithm) [[0 0]])))
      (is (= [[1 1] [2 2]] ((:get-window algorithm) [[0 0] [1 1] [2 2]])))
      (let [result ((:process algorithm) algorithm [[0 0] [1 1]] 0.5 nil)]
        (is (some? result))
        (is (some? (:last-x result)))
        (is (pos? (count (:pts result))))))))

(deftest test-newton-algorithm
  (testing "Алгоритм интерполяции Ньютона"
    (let [algorithm (create-newton-algorithm 4)]
      (is (= 4 ((:window-size algorithm))))
      (is (= "newton" ((:algorithm-name algorithm))))
      (is (true? ((:can-process? algorithm) [[0 0] [1 1] [2 2] [3 3] [4 4]])))
      (is (false? ((:can-process? algorithm) [[0 0] [1 1] [2 2] [3 3]])))
      (is (= [[1 1] [2 2] [3 3] [4 4]] ((:get-window algorithm) [[0 0] [1 1] [2 2] [3 3] [4 4] [5 5]])))
      (let [result ((:process algorithm) algorithm [[0 0] [1 1] [2 4] [3 9]] 0.5 nil)]
        (is (some? result))
        (is (some? (:last-x result)))
        (is (pos? (count (:pts result))))))))

(deftest test-compute-linear-points
  (testing "Генерация точек для линейной интерполяции"
    (let [window [[0 0] [1 1]]
          step 0.5]
      (is (vector? (compute-linear-points window step nil)))
      (is (pos? (count (compute-linear-points window step nil))))
      (is (= [0.5 1.0] (compute-linear-points window step 0.0))))))

(deftest test-divided-diffs
  (testing "Вычисление разделенных разностей"
    (let [points [[0 0] [1 1] [2 4]]
          diffs (divided-diffs points)]
      (is (vector? diffs))
      (is (= 3 (count diffs)))
      (is (= [0 1 4] (first diffs))))))

(deftest test-linear-range
  (testing "Определение диапазона для линейной интерполяции"
    (let [window [[0 0] [1 1] [2 2]]]
      (is (= [1 2] (linear-range window))))))

(deftest test-newton-range
  (testing "Определение диапазона для интерполяции Ньютона"
    (let [window [[0 0] [1 1] [2 4] [3 9]]]
      (is (= [0 3] (newton-range window))))))
