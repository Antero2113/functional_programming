(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

;; Тесты для НОД
(deftest gcd-test
  (testing "НОД различных чисел"
    (is (= 6 (gcd 54 24)))
    (is (= 1 (gcd 17 13)))
    (is (= 5 (gcd 15 10)))))

(deftest lcm-test
  (testing "НОК различных чисел"
    (is (= 36 (lcm 12 18)))
    (is (= 91 (lcm 7 13)))
    (is (= 60 (lcm 15 20)))))

(deftest lcm-edge-cases-test
  (testing "Крайние случаи НОК"
    (is (= 0 (lcm 0 5)))
    (is (= 0 (lcm 7 0)))
    (is (= 0 (lcm 0 0)))
    (is (= 1 (lcm 1 1)))
    (is (= 100 (lcm 1 100)))))

(deftest smallest-multiple-test
  (testing "Наименьшее число, делящееся на диапазон"
    (is (= 2520 (smallest-multiple-modular 10)))
    (is (= 232792560 (smallest-multiple-modular 20)))))

(deftest edge-cases-test
  (testing "Крайние случаи"
    (is (= 1 (smallest-multiple-tail 1)))
    (is (= 2 (smallest-multiple-tail 2)))
    (is (= 6 (smallest-multiple-tail 3)))))

;; Тесты для длины цикла
(deftest cycle-length-test
  (testing "Длина цикла для различных знаменателей"
    (is (= 0 (cycle-length 2)))   ; 1/2 = 0.5 - конечная дробь
    (is (= 0 (cycle-length 5)))   ; 1/5 = 0.2 - конечная дробь
    (is (= 1 (cycle-length 3)))   ; 1/3 = 0.(3) - период 1
    (is (= 1 (cycle-length 6)))   ; 1/6 = 0.1(6) - период 1
    (is (= 6 (cycle-length 7))))) ; 1/7 = 0.(142857) - период 6

(deftest cycle-length-edge-cases-test
  (testing "Крайние случаи длины цикла"
    (is (thrown? IllegalArgumentException (cycle-length 1)))
    (is (thrown? IllegalArgumentException (cycle-length 0)))
    (is (thrown? IllegalArgumentException (cycle-length -5)))))

;; Тесты для задачи 26
(deftest euler-26-consistency-test
  (testing "Все версии euler-26 дают одинаковые результаты"
    (doseq [limit [10 20 50]]
      (let [results [(euler-26-tail-recursion limit)
                     (euler-26-recursion limit)
                     (euler-26-modular limit)
                     (euler-26-map limit)
                     (euler-26-loop limit)
                     (euler-26-lazy limit)]]
        (is (apply = results) (str "Все результаты для limit=" limit " должны совпадать: " results))))))

(deftest euler-26-values-test
  (testing "Значения для задачи 26"
    (is (= 7 (euler-26-tail-recursion 10)))   ; 1/7 имеет период 6
    (is (= 7 (euler-26-recursion 10)))
    (is (= 7 (euler-26-modular 10)))
    (is (= 7 (euler-26-map 10)))
    (is (= 7 (euler-26-loop 10)))
    (is (= 7 (euler-26-lazy 10)))))

(deftest euler-26-edge-cases-test
  (testing "Крайние случаи для задачи 26"
    (is (= 0 (euler-26-tail-recursion 2)))   ; нет чисел с циклом > 0
    (is (= 0 (euler-26-recursion 2)))
    (is (= 0 (euler-26-modular 2)))
    (is (= 0 (euler-26-map 2)))
    (is (= 0 (euler-26-loop 2)))
    (is (= 0 (euler-26-lazy 2)))
    
    (is (= 3 (euler-26-tail-recursion 4)))))   ; 1/3 имеет период 1

;; Интеграционные тесты
(deftest integration-test
  (testing "Интеграция различных функций"
    (let [n 10
          multiple (smallest-multiple-modular n)
          gcd-result (gcd multiple (inc multiple))
          lcm-result (lcm multiple 2)]
      (is (= 1 gcd-result)) ; НОД последовательных чисел всегда 1
      (is (= multiple lcm-result))))) ; НОК(multiple, 2) = multiple, т.к. multiple уже кратно 2