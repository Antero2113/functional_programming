(ns sc-bag.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sc-bag.core :refer :all]))

;; Unit Tests

(deftest basic-operations-test
  (testing "Создание пустого bag"
    (let [b (empty-bag)]
      (is (zero? (count-elements b)))
      (is (zero? (distinct-count b)))
      (is (zero? (get-count b :anything)))))

  (testing "Добавление элементов"
    (let [b (-> (empty-bag)
                (bag-conj :a)
                (bag-conj :b)
                (bag-conj :a))]
      (is (= 3 (count-elements b)))
      (is (= 2 (distinct-count b)))
      (is (= 2 (get-count b :a)))
      (is (= 1 (get-count b :b)))
      (is (bag-contains? b :a))
      (is (not (bag-contains? b :c)))))

  (testing "Удаление элементов"
    (let [b (-> (empty-bag)
                (bag-conj :a)
                (bag-conj :a)
                (bag-conj :b)
                (bag-disj :a))]
      (is (= 2 (count-elements b)))
      (is (= 2 (distinct-count b)))
      (is (= 1 (get-count b :a)))
      (is (= 1 (get-count b :b)))

      (let [b' (bag-disj b :c)]  ;; Удаление несуществующего элемента
        (is (bag-equals? b b'))))))

(deftest collection-creation-test
  (testing "Создание bag из коллекции"
    (let [b (bag [1 2 3 2 1])]
      (is (= 5 (count-elements b)))
      (is (= 3 (distinct-count b)))
      (is (= 2 (get-count b 1)))
      (is (= 2 (get-count b 2)))
      (is (= 1 (get-count b 3)))
      (is (bag-contains? b 1))
      (is (not (bag-contains? b 4))))))

(deftest higher-order-functions-test
  (testing "Фильтрация"
    (let [b (bag [1 2 3 2 4 1 5])
          filtered (bag-filter even? b)]
      (is (= 3 (count-elements filtered)))
      (is (= 2 (get-count filtered 2)))
      (is (= 1 (get-count filtered 4)))
      (is (zero? (get-count filtered 1)))
      (is (zero? (get-count filtered 3)))
      (is (zero? (get-count filtered 5)))))

  (testing "Map"
    (let [b (bag [1 2 3])
          mapped (bag-map inc b)]
      (is (= 3 (count-elements mapped)))
      (is (= 1 (get-count mapped 2)))
      (is (= 1 (get-count mapped 3)))
      (is (= 1 (get-count mapped 4)))))

  (testing "Свертки"
    (let [b (bag [1 2 3 2])]
      (is (= 8 (reduce-left + 0 b)))
      (is (= 8 (reduce-right + 0 b)))
      ;; Проверяем что все элементы присутствуют в правильном количестве
      (let [elements (bag-seq b)
            freqs (frequencies elements)]
        (is (= 4 (count elements)))
        (is (= 1 (get freqs 1)))
        (is (= 2 (get freqs 2)))
        (is (= 1 (get freqs 3)))))))

(deftest monoid-test
  (testing "Моноидные свойства"
    (let [empty-b (empty-bag)
          b1 (bag [1 2 2 3])
          b2 (bag [2 3 4])
          b3 (bag [1 4 4])]

      ;; Левая идентичность
      (testing "Левая идентичность"
        (is (bag-equals? (bag-union empty-b b1) b1)))

      ;; Правая идентичность  
      (testing "Правая идентичность"
        (is (bag-equals? (bag-union b1 empty-b) b1)))

      ;; Ассоциативность
      (testing "Ассоциативность"
        (is (bag-equals? (bag-union (bag-union b1 b2) b3)
                         (bag-union b1 (bag-union b2 b3)))))

      ;; Пример объединения
      (testing "Пример объединения"
        (let [union-result (bag-union (bag [1 2]) (bag [2 3]))]
          (is (= 4 (count-elements union-result)))  ;; 1 + 2 + 1 = 4
          (is (= 1 (get-count union-result 1)))
          (is (= 2 (get-count union-result 2)))
          (is (= 1 (get-count union-result 3))))))))

(deftest equality-test
  (testing "Эффективное сравнение"
    (let [b1 (bag [1 2 2 3])
          b2 (bag [2 1 3 2])  ;; Другой порядок
          b3 (bag [1 2 3])    ;; Меньше элементов
          b4 (bag [1 2 2 3 4]) ;; Больше элементов
          b5 (bag [1 2 3 3])  ;; Другое распределение
          empty-b (empty-bag)]

      (is (bag-equals? b1 b2))
      (is (not (bag-equals? b1 b3)))
      (is (not (bag-equals? b1 b4)))
      (is (not (bag-equals? b1 b5)))
      (is (bag-equals? empty-b empty-b))
      (is (not (bag-equals? b1 empty-b))))))

;; Property-based Tests

(def element-gen (gen/one-of [gen/int gen/boolean gen/keyword]))
(def small-vector-gen (gen/vector element-gen 0 5))

(defspec monoid-associativity 50
  (prop/for-all [v1 small-vector-gen
                 v2 small-vector-gen
                 v3 small-vector-gen]
                (let [b1 (bag v1)
                      b2 (bag v2)
                      b3 (bag v3)]
                  (bag-equals? (bag-union (bag-union b1 b2) b3)
                               (bag-union b1 (bag-union b2 b3))))))

(defspec monoid-identity 50
  (prop/for-all [v small-vector-gen]
                (let [b (bag v)
                      empty-b (empty-bag)]
                  (and (bag-equals? (bag-union empty-b b) b)
                       (bag-equals? (bag-union b empty-b) b)))))

(defspec count-consistency 50
  (prop/for-all [v small-vector-gen]
                (let [b (bag v)]
                  (and (= (count v) (count-elements b))
                       (= (count (distinct v)) (distinct-count b))))))

(defspec add-remove-consistency 50
  (prop/for-all [v small-vector-gen
                 elem element-gen]
                (let [b1 (bag v)
                      b2 (bag-conj b1 elem)]
                  (if (some #(= % elem) v)
                    (bag-equals? b1 (bag-disj b2 elem))
                    (= (get-count b1 elem) (dec (get-count b2 elem)))))))
