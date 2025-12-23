(ns interpolation.core-test
  (:require [clojure.test :refer :all]
            [interpolation.core :refer :all]))

(deftest test-parse-line
  (testing "Корректный парсинг входных строк"
    (is (= [0.0 0.0] (parse-line "0 0")))
    (is (= [1.0 2.0] (parse-line "1 2")))
    (is (= [1.5 2.5] (parse-line "1.5 2.5")))
    (is (= [1.0 2.0] (parse-line "1;2")))
    (is (= [1.0 2.0] (parse-line "1\t2")))
    (is (nil? (parse-line "invalid")))
    (is (nil? (parse-line "1 2 3")))))

(deftest test-valid-x
  (testing "Проверка монотонности x"
    (is (true? (valid-x? [] [0 0])))
    (is (true? (valid-x? [[0 0]] [1 1])))
    (is (true? (valid-x? [[0 0]] [0 1])))
    (is (false? (valid-x? [[1 1]] [0 0])))))

(deftest test-xs-between
  (testing "Генерация x с учетом шага и last-x"
    (is (= [0.0 1.0 2.0]
           (take 3 (xs-between 0.0 2.0 1.0 nil))))
    (is (= [1.5 2.0]
           (take 2 (xs-between 0.0 2.0 0.5 1.0))))))

(deftest test-linear-interp
  (testing "Линейная интерполяция между двумя точками"
    (let [p1 [0.0 0.0]
          p2 [1.0 1.0]]
      (is (= 0.0 (linear-interp [p1 p2] 0.0)))
      (is (= 0.5 (linear-interp [p1 p2] 0.5)))
      (is (= 1.0 (linear-interp [p1 p2] 1.0))))))

(deftest test-process-linear
  (testing "Потоковая линейная интерполяция"
    (let [buffer [[0 0] [1 1]]
          step 0.5
          res (process-linear buffer step nil)]
      (is (some? res))
      (is (= 1.0 (:last-x res)))
      (is (every? true?
                  (map (fn [[x y] [ex ey]]
                         (and (== x ex) (== y ey)))
                       (:out res)
                       [[0.0 0.0] [0.5 0.5] [1.0 1.0]]))))))

(deftest test-divided-diffs
  (testing "Разделённые разности Ньютона"
    (let [pts [[0 0] [1 1] [2 4]]
          diffs (divided-diffs pts)]
      (is (= 3 (count diffs)))
      (is (= [0 1 4] (first diffs))))))

(deftest test-newton-fn
  (testing "Полином Ньютона проходит через заданные точки"
    (let [pts [[0 0] [1 1] [2 4] [3 9]]
          f   (newton-fn pts)]
      (is (= 0.0 (f 0.0)))
      (is (= 1.0 (f 1.0)))
      (is (= 4.0 (f 2.0)))
      (is (= 9.0 (f 3.0)))
      (is (number? (f 1.5))))))

(deftest test-process-newton
  (testing "Потоковая интерполяция Ньютона (n точек, триггер n+1)"
    (let [buffer [[0 0] [1 1] [2 4] [3 9] [4 16]]
          n 4
          step 0.5
          res (process-newton buffer n step nil)]
      (is (some? res))
      (is (= 3.0 (:last-x res)))
      (is (some #(= [1.0 1.0] %) (:out res)))
      (is (some #(= [2.0 4.0] %) (:out res))))))

(deftest test-insufficient-data
  (testing "Недостаточно данных для интерполяции"
    (is (nil? (process-linear [[0 0]] 0.5 nil)))
    (is (nil? (process-newton [[0 0] [1 1] [2 2]] 4 0.5 nil)))))
