(ns interpolation.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
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

(deftest test-process-linear
  (testing "Обработка линейной интерполяции"
    (let [algorithm (->LinearInterpolation)
          window [[0 0] [1 1]]
          step 0.5
          result (process algorithm window step nil)]
      (is (not (nil? result)))
      (is (some? (:last-x result)))
      (is (pos? (count (:pts result))))
      (let [first-pt (first (:pts result))
            last-pt (last (:pts result))]
        (is (vector? first-pt))
        (is (vector? last-pt))
        (is (= 2 (count first-pt)))
        (is (= 2 (count last-pt)))))))

(deftest test-process-newton
  (testing "Обработка интерполяции Ньютона"
    (let [algorithm (->NewtonInterpolation 4)
          window [[0 0] [1 1] [2 4] [3 9]]
          step 0.5
          result (process algorithm window step nil)]
      (is (not (nil? result)))
      (is (some? (:last-x result)))
      (is (pos? (count (:pts result)))))))

(deftest test-compute-linear
  (testing "Генерация точек для линейной интерполяции"
    (let [window [[0 0] [1 1]]
          step 0.5
          xs (compute-linear-points window step nil)]
      (is (vector? xs))
      (is (pos? (count xs)))
      (is (<= (first xs) (last xs)))
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

(deftest test-linear-algorithm-protocol
  (testing "Протокол для линейной интерполяции"
    (let [algorithm (->LinearInterpolation)]
      (is (= 2 (window-size algorithm)))
      (is (= "linear" (algorithm-name algorithm)))
      (is (can-process? algorithm [[0 0] [1 1]]))
      (is (not (can-process? algorithm [[0 0]])))
      (is (= [[1 1] [2 2]] (get-window algorithm [[0 0] [1 1] [2 2]]))))))

(deftest test-newton-algorithm-protocol
  (testing "Протокол для интерполяции Ньютона"
    (let [algorithm (->NewtonInterpolation 4)]
      (is (= 4 (window-size algorithm)))
      (is (= "newton" (algorithm-name algorithm)))
      (is (can-process? algorithm [[0 0] [1 1] [2 2] [3 3]]))
      (is (not (can-process? algorithm [[0 0] [1 1] [2 2]])))
      (is (= [[1 1] [2 2] [3 3] [4 4]] (get-window algorithm [[0 0] [1 1] [2 2] [3 3] [4 4]]))))))
