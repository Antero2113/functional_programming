(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

;; Тесты для НОД и НОК
(deftest gcd-test
  (testing "Алгоритм Евклида для НОД"
    (is (= 6 (gcd 48 18)))
    (is (= 1 (gcd 17 13)))
    (is (= 5 (gcd 15 10)))
    (is (= 4 (gcd 12 8)))
    (is (= 12 (gcd 36 24)))))

(deftest lcm-test
  (testing "Вычисление НОК"
    (is (= 36 (lcm 12 18)))
    (is (= 0 (lcm 0 5)))
    (is (= 0 (lcm 5 0)))
    (is (= 15 (lcm 3 5)))
    (is (= 60 (lcm 12 15)))))

;; Тесты для задачи 5 - Smallest Multiple
(deftest smallest-multiple-test
  (let [expected-10 2520   ; Известный результат для 1-10
        expected-20 232792560]  ; Ожидаемый результат для 1-20
    
    (testing "Проверка для n=10 (базовый случай)"
      (is (= expected-10 (smallest-multiple-tail 10)))
      (is (= expected-10 (smallest-multiple-rec 10)))
      (is (= expected-10 (smallest-multiple-modular 10)))
      (is (= expected-10 (smallest-multiple-map 10)))
      (is (= expected-10 (smallest-multiple-for 10)))
      (is (= expected-10 (smallest-multiple-lazy-infinite 10))))
    
    (testing "Проверка для n=20 (основной случай)"
      (is (= expected-20 (smallest-multiple-tail 20)))
      (is (= expected-20 (smallest-multiple-rec 20)))
      (is (= expected-20 (smallest-multiple-modular 20)))
      (is (= expected-20 (smallest-multiple-map 20)))
      (is (= expected-20 (smallest-multiple-for 20)))
      (is (= expected-20 (smallest-multiple-lazy-infinite 20))))
    
    (testing "Все методы дают одинаковый результат"
      (let [results [(smallest-multiple-tail 20)
                     (smallest-multiple-rec 20)
                     (smallest-multiple-modular 20)
                     (smallest-multiple-map 20)
                     (smallest-multiple-for 20)
                     (smallest-multiple-lazy-infinite 20)]]
        (is (apply = results))))))

;; Тесты для prime-factors
(deftest prime-factors-test
  (testing "Разложение на простые множители"
    (is (= [2 2 3] (prime-factors 12)))
    (is (= [2 3 5] (prime-factors 30)))
    (is (= [7] (prime-factors 7)))
    (is (= [2 2 2 3] (prime-factors 24)))
    (is (= [] (prime-factors 1)))))

;; Тесты для задачи 26 - Reciprocal Cycles
(deftest cycle-length-test
  (testing "Длина цикла для различных знаменателей"
    (is (= 0 (cycle-length 1)))     ; 1/1 = 1.0 - нет цикла
    (is (= 0 (cycle-length 2)))     ; 1/2 = 0.5 - нет цикла
    (is (= 1 (cycle-length 3)))     ; 1/3 = 0.(3) - цикл длины 1
    (is (= 0 (cycle-length 4)))     ; 1/4 = 0.25 - нет цикла
    (is (= 0 (cycle-length 5)))     ; 1/5 = 0.2 - нет цикла
    (is (= 1 (cycle-length 6)))     ; 1/6 = 0.1(6) - цикл длины 1
    (is (= 6 (cycle-length 7)))     ; 1/7 = 0.(142857) - цикл длины 6
    (is (= 0 (cycle-length 8)))     ; 1/8 = 0.125 - нет цикла
    (is (= 1 (cycle-length 9)))     ; 1/9 = 0.(1) - цикл длины 1
    (is (= 0 (cycle-length 10)))))  ; 1/10 = 0.1 - нет цикла

(deftest euler-26-test
  (let [expected-10 7   ; Для limit=10 максимальная длина цикла у 7
        expected-100 97] ; Для limit=100 максимальная длина цикла у 97
    
    (testing "Проверка для limit=10"
      (is (= expected-10 (euler-26-tail-recursion 10)))
      (is (= expected-10 (euler-26-recursion 10)))
      (is (= expected-10 (euler-26-modular-map 10)))
      (is (= expected-10 (euler-26-for 10)))
      (is (= expected-10 (euler-26-lazy-infinite 10))))
    
    (testing "Проверка для limit=100"
      (is (= expected-100 (euler-26-tail-recursion 100)))
      (is (= expected-100 (euler-26-recursion 100)))
      (is (= expected-100 (euler-26-modular-map 100)))
      (is (= expected-100 (euler-26-for 100)))
      (is (= expected-100 (euler-26-lazy-infinite 100))))
    
    (testing "Все методы дают одинаковый результат для limit=20"
      (let [results [(euler-26-tail-recursion 20)
                     (euler-26-recursion 20)
                     (euler-26-modular-map 20)
                     (euler-26-for 20)
                     (euler-26-lazy-infinite 20)]]
        (is (apply = results))))))

;; Тесты граничных случаев
(deftest edge-cases-test
  (testing "Граничные случаи для smallest-multiple"
    (is (= 1 (smallest-multiple-tail 1)))
    (is (= 2 (smallest-multiple-tail 2)))
    (is (= 6 (smallest-multiple-tail 3))))
  
  (testing "Граничные случаи для cycle-length"
    (is (= 0 (cycle-length 0)))     ; Обработка нуля
    (is (= 0 (cycle-length -1)))    ; Обработка отрицательных чисел
    (is (= 0 (cycle-length 1))))    ; Обработка единицы
  
  (testing "Граничные случаи для euler-26"
    (is (= 0 (euler-26-tail-recursion 2)))  ; limit=2, только d=1 (не рассматривается)
    (is (= 0 (euler-26-tail-recursion 3)))  ; limit=3, d=2 (длина цикла 0)
    (is (= 3 (euler-26-tail-recursion 4)))) ; limit=4, d=3 (длина цикла 1) - но 3 > 2
)

;; Тесты производительности с измерением времени всех реализаций
(deftest performance-smallest-multiple-test
  (testing "Производительность всех реализаций Smallest Multiple (n=20)"
    (println "\n=== Производительность Smallest Multiple (n=20) ===")
    (println "1.1. Хвостовая рекурсия:        " (time (smallest-multiple-tail 20)))
    (println "1.2. Обычная рекурсия:          " (time (smallest-multiple-rec 20)))
    (println "2. Модульная версия (reduce): " (time (smallest-multiple-modular 20)))
    (println "3. С отображением (map):      " (time (smallest-multiple-map 20)))
    (println "4. Со спец. синтаксисом (for):" (time (smallest-multiple-for 20)))
    (println "5. Ленивые списки:            " (time (smallest-multiple-lazy-infinite 20)))
    (is true))) ; Проверяем, что все методы завершаются

(deftest performance-euler-26-test
  (testing "Производительность всех реализаций Reciprocal Cycles (limit=1000)"
    (println "\n=== Производительность Reciprocal Cycles (limit=1000) ===")
    (println "1.1. Хвостовая рекурсия:        " (time (euler-26-tail-recursion 1000)))
    (println "1.2. Обычная рекурсия:          " (time (euler-26-recursion 1000)))
    (println "2-3. Модульная версия (reduce) с map:   " (time (euler-26-modular-map 1000)))
    (println "4. Со спец. синтаксисом (for): " (time (euler-26-for 1000)))
    (println "5. Ленивые списки:             " (time (euler-26-lazy-infinite 1000)))
    (is true))) ; Проверяем, что все методы завершаются

;; Запуск всех тестов
(defn test-ns-hook
  []
  (gcd-test)
  (lcm-test)
  (smallest-multiple-test)
  (prime-factors-test)
  (cycle-length-test)
  (euler-26-test)
  (edge-cases-test)
  ( performance-smallest-multiple-test)
  (performance-euler-26-test))