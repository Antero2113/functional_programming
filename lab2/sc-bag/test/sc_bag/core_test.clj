(ns sc-bag.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [sc-bag.core :refer :all :as core]))

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

(deftest seq-functions-test
  (testing "bag-seq возвращает seqable коллекцию"
    (let [b (bag [1 2 3])
          result (bag-seq b)]
      (is (seqable? result))
      (is (not (integer? result)))
      (is (= 3 (count result)))))

  (testing "bag-distinct-seq возвращает seqable коллекцию"
    (let [b (bag [1 2 2 3])
          result (bag-distinct-seq b)]
      (is (seqable? result))
      (is (not (integer? result)))
      (is (= 3 (count result)))))

  (testing "count-elements возвращает число, а не коллекцию"
    (let [b (bag [1 2 3])
          result (count-elements b)]
      (is (integer? result))
      (is (not (seqable? result)))))

  (testing "bag-contains? возвращает boolean"
    (let [b (bag [1 2 3])
          result (bag-contains? b 2)]
      (is (boolean? result))
      (is (not (seqable? result)))))

  (testing "Функции работают корректно с обычными коллекциями"
    (let [coll [1 2 2 3]]
      (is (seqable? (bag-seq coll)))
      (is (= 4 (count-elements coll)))
      (is (bag-contains? coll 2))
      (is (not (bag-contains? coll 4))))))

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

(deftest collection-interfaces-test
  (testing "Интерфейс Seqable"
    (let [b (bag [1 2 3 2])]
      (is (seqable? b))
      (is (seq b))
      (is (= 4 (count (seq b))))
      ;; Порядок элементов может быть разным, проверяем через frequencies
      (is (= {1 1, 2 2, 3 1} (frequencies (seq b))))))

  (testing "Интерфейс IPersistentCollection"
    (let [b (bag [1 2 3])]
      (is (= 3 (count b)))
      ;; cons на bag возвращает bag (реализация IPersistentCollection)
      (let [b-with-4 (cons b 4)]
        (is (= 4 (count b-with-4))))
      (is (empty? (empty b)))
      (is (not (empty? b)))
      (is (= b (bag [3 1 2])))
      (is (not (= b (bag [1 2 3 4]))))))

  (testing "Интерфейс ILookup"
    (let [b (bag [1 2 2 3])]
      (is (= 1 (get b 1)))
      (is (= 2 (get b 2)))
      (is (= 1 (get b 3)))
      (is (= 0 (get b 4)))
      (is (= :not-found (get b 5 :not-found)))
      ;; Для работы с bag как функцией
      (is (= 2 (b 2)))
      (is (= :not-found (b 5 :not-found)))
      ;; Проверяем, что доступ к полям записи все еще работает
      (is (some? (get b :buckets)))
      (is (some? (get b :size)))))

  (testing "Интерфейс Associative"
    (let [b (bag [1 2 3])]
      (is (contains? b 1))
      (is (contains? b 2))
      (is (not (contains? b 4)))
      ;; Проверяем, что доступ к полям записи все еще работает
      (is (contains? b :buckets))
      (is (contains? b :size))
      ;; Для работы с entryAt
      (let [entry (find b 2)]
        (is (some? entry))
        (is (= 2 (key entry)))
        (is (= 1 (val entry))))
      ;; Для добавления элементов используем cons
      ;; cons на bag возвращает bag (реализация IPersistentCollection)
      (let [b2 (cons b 4)]
        (is (= 1 (get b2 4))))
      ;; cons на bag возвращает bag (реализация IPersistentCollection)
      (let [b3 (loop [result b n 2]
                 (if (zero? n)
                   result
                   (recur (cons result 1) (dec n))))]
        (is (= 3 (get b3 1))))))

  (testing "Работа со стандартными функциями Clojure"
    (let [b (bag [1 2 3 2 1])]
      (is (= 5 (count b)))
      (is (= 3 (count (distinct b))))
      (is (some #{2} b))
      (is (every? integer? b))
      (is (= #{1 2 3} (set b)))
      (is (= [1 1 2 2 3] (sort (seq b)))))))

;; Property-based Tests

(def element-gen (gen/one-of [gen/int gen/boolean gen/keyword]))

;; Генератор bag-ов через последовательные операции
(def bag-gen
  (gen/let [elements (gen/vector element-gen 0 10)]
    (reduce bag-conj (empty-bag) elements)))

;; Генератор непустых bag-ов
(def non-empty-bag-gen
  (gen/let [elements (gen/vector element-gen 1 10)]
    (reduce bag-conj (empty-bag) elements)))

(defspec bag-seq-returns-seqable 50
  (prop/for-all [b bag-gen]
                (let [result (bag-seq b)]
                  (and (seqable? result)
                       (not (integer? result))))))

(defspec bag-distinct-seq-returns-seqable 50
  (prop/for-all [b bag-gen]
                (let [result (bag-distinct-seq b)]
                  (and (seqable? result)
                       (not (integer? result))))))

(defspec monoid-associativity 50
  (prop/for-all [b1 bag-gen
                 b2 bag-gen
                 b3 bag-gen]
                (bag-equals? (bag-union (bag-union b1 b2) b3)
                             (bag-union b1 (bag-union b2 b3)))))

(defspec monoid-identity 50
  (prop/for-all [b bag-gen]
                (let [empty-b (empty-bag)]
                  (and (bag-equals? (bag-union empty-b b) b)
                       (bag-equals? (bag-union b empty-b) b)))))

(defspec count-consistency 50
  (prop/for-all [elements (gen/vector element-gen 0 10)]
                (let [b (reduce bag-conj (empty-bag) elements)]
                  (and (= (count elements) (count-elements b))
                       (= (count (distinct elements)) (distinct-count b))))))

(defspec add-remove-consistency 50
  (prop/for-all [b bag-gen
                 elem element-gen]
                (let [b-with-elem (bag-conj b elem)
                      cnt-before (get-count b elem)]
                  (if (pos? cnt-before)
                    (bag-equals? b (bag-disj b-with-elem elem))
                    (= cnt-before (dec (get-count b-with-elem elem)))))))

(defspec filter-consistency 50
  (prop/for-all [b bag-gen]
                (let [filtered (bag-filter even? b)
                      all-elements (bag-seq b)
                      expected-count (count (filter even? all-elements))]
                  (= expected-count (count-elements filtered)))))

(defspec map-consistency 50
  (prop/for-all [b bag-gen]
                (let [mapped (bag-map inc b)
                      all-elements (bag-seq b)
                      expected-elements (map inc all-elements)]
                  (bag-equals? mapped (reduce bag-conj (empty-bag) expected-elements)))))

(defspec bag-generator-test 50
  (prop/for-all [b bag-gen]
                (and (>= (count-elements b) 0)
                     (>= (distinct-count b) 0)
                     (<= (distinct-count b) (count-elements b))
                     (instance? sc_bag.core.SCBag b))))

(defspec bag-interface-consistency 50
  (prop/for-all [b bag-gen
                 elem element-gen]
                (let [cnt (get-count b elem)]
                  (and (>= cnt 0)
                       (= (bag-contains? b elem) (pos? cnt))
                       (= cnt (b elem))
                       (= cnt (get b elem))))))

(defspec bag-reduce-consistency 50
  (prop/for-all [b bag-gen]
                (let [elements (bag-seq b)
                      left-reduce (reduce-left conj [] b)
                      right-reduce (reduce-right conj [] b)]
                  (and (= (count elements) (count left-reduce))
                       (= (count elements) (count right-reduce))
                       (= (frequencies elements) (frequencies left-reduce))
                       (= (frequencies elements) (frequencies right-reduce))))))

(defspec bag-disj-consistency 50
  (prop/for-all [b non-empty-bag-gen]
                (let [distinct-elems (bag-distinct-seq b)
                      elem (first distinct-elems)
                      b-after-disj (bag-disj b elem)
                      cnt-before (get-count b elem)]
                  (if (> cnt-before 1)
                    (= (dec cnt-before) (get-count b-after-disj elem))
                    (not (bag-contains? b-after-disj elem))))))

(defspec bag-unique-properties 50
  (prop/for-all [elements (gen/vector element-gen 0 15)]
                (let [b (reduce bag-conj (empty-bag) elements)
                      distinct-elems (bag-distinct-seq b)
                      frequencies-map (bag-frequencies b)]
                  
                  (and
                   ;; Количество уникальных элементов должно совпадать
                   (= (count distinct-elems) (distinct-count b))
                   
                   ;; Сумма частот должна равняться общему количеству элементов
                   (= (count-elements b) (reduce + (vals frequencies-map)))
                   
                   ;; Каждый уникальный элемент должен иметь правильное количество
                   (every? (fn [elem]
                             (= (get-count b elem)
                                (get frequencies-map elem)))
                           distinct-elems)
                   
                   ;; Bag должен быть равен самому себе
                   (bag-equals? b b)))))

(defspec bag-empty-properties 50
  (prop/for-all [elem element-gen]
                (let [empty-b (empty-bag)]
                  (and (zero? (count-elements empty-b))
                       (zero? (distinct-count empty-b))
                       (zero? (get-count empty-b elem))
                       (not (bag-contains? empty-b elem))
                       (empty? (bag-seq empty-b))
                       (empty? (bag-distinct-seq empty-b))))))

(defspec bag-disj-first-element 50
  (prop/for-all [b non-empty-bag-gen]
                (let [distinct-elements (bag-distinct-seq b)
                      first-elem (first distinct-elements)
                      b-after-disj (bag-disj b first-elem)
                      original-count (get-count b first-elem)]
                  (if (> original-count 1)
                    (= (dec original-count) (get-count b-after-disj first-elem))
                    (not (contains? b-after-disj first-elem))))))

(defspec bag-union-frequencies 50
  (prop/for-all [b1 bag-gen
                 b2 bag-gen]
                (let [union-bag (bag-union b1 b2)
                      all-elements (concat (bag-seq b1) (bag-seq b2))
                      expected-frequencies (frequencies all-elements)]
                  
                  (every? (fn [[elem expected-count]]
                            (= expected-count (get-count union-bag elem)))
                          expected-frequencies))))

;; Edge cases and special scenarios

(deftest edge-cases-test
  (testing "Работа с nil значениями"
    (let [b (bag [nil 1 nil 2])]
      (is (= 4 (count-elements b)))
      (is (= 3 (distinct-count b)))
      (is (= 2 (get-count b nil)))))

  (testing "Работа с большим количеством элементов"
    (let [elements (repeat 100 :a)
          b (bag elements)]
      (is (= 100 (count-elements b)))
      (is (= 1 (distinct-count b)))
      (is (= 100 (get-count b :a)))))

  (testing "Работа с различными типами данных"
    (let [b (bag [1 :a "string" true false nil 1 :a])]
      (is (= 8 (count-elements b)))
      (is (= 6 (distinct-count b)))
      (is (= 2 (get-count b 1)))
      (is (= 2 (get-count b :a)))
      (is (= 1 (get-count b "string")))))

  (testing "Множественное удаление элементов"
    (let [b (-> (empty-bag)
                (bag-conj :a)
                (bag-conj :a)
                (bag-conj :a)
                (bag-disj :a)
                (bag-disj :a))]
      (is (= 1 (count-elements b)))
      (is (= 1 (distinct-count b)))
      (is (= 1 (get-count b :a))))))

;; Performance and stress tests

(deftest performance-test
  (testing "Создание bag из большой коллекции"
    (let [large-coll (range 1000)
          b (bag large-coll)]
      (is (= 1000 (count-elements b)))
      (is (= 1000 (distinct-count b)))))

  (testing "Множественные операции добавления/удаления"
    (let [b (reduce bag-conj (empty-bag) (range 100))]
      (is (= 100 (count-elements b)))
      (let [b' (reduce bag-disj b (range 50))]
        (is (= 50 (count-elements b')))
        (is (= 100 (distinct-count b')))))))
