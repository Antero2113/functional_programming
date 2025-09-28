(ns clojure-lab1.core-test
  (:require [clojure.test :refer :all]
            [clojure-lab1.core :refer :all]))

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

(deftest smallest-multiple-test
  (testing "Наименьшее число, делящееся на диапазон"
    (is (= 2520 (smallest-multiple 10)))
    (is (= 232792560 (smallest-multiple 20)))))

(deftest edge-cases-test
  (testing "Крайние случаи"
    (is (= 1 (smallest-multiple 1)))
    (is (= 2 (smallest-multiple 2)))
    (is (= 6 (smallest-multiple 3)))))
