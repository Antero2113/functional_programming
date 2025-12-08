# Лабораторная работа №1
Карандашева Анастасия, 368273, группа P3332

Цель: освоить базовые приёмы и абстракции функционального программирования: функции, поток управления и поток данных, сопоставление с образцом, рекурсия, свёртка, отображение, работа с функциями как с данными, списки.

Индивидуальный вариант: задачи 5 и 26 [проекта Эйлер](https://projecteuler.net/archives).

## Описание задач и основных алгоритмов их решения

### Задача 5 

**Условие:**  
2520 – это наименьшее число, которое делится без остатка на все числа от 1 до 10.  
Какое наименьшее положительное число делится без остатка на все числа от 1 до 20?

**Общий алгоритм:**  
1. **Разложить на простые множители:** Для каждого числа от 1 до 20 разложить его на простые множители (например, 12 = 2² × 3¹).  
2. **Найти НОК:** Взять максимальные степени всех простых чисел, встречающихся в разложениях. Например, для простого числа 2 максимальная степень в диапазоне 1-20 — 2⁴ (так как 16 = 2⁴).  
3. **Вычислить произведение:** Умножить все эти простые числа в максимальных степенях. Формула:  
   НОК(1, 2, ..., 20) = ∏ p^max(k)  
   где p – простое число, а max(k) – его наибольшая степень, не превышающая 20.

**Ожидаемый ответ:**  
232792560


### Задача 26 

**Условие:**  
Дробь с единицей в числителе называется единичной дробью. Десятичные представления единичных дробей со знаменателями от 2 до 10:

```
1/2 = 0.5
1/3 = 0.(3)
1/4 = 0.25
1/5 = 0.2
1/6 = 0.1(6)
1/7 = 0.(142857)
1/8 = 0.125
1/9 = 0.(1)
1/10 = 0.1
```

Здесь 0.1(6) означает 0.166666... и имеет период длины 1. Видно, что 1/7 имеет период длины 6.  
Найдите значение d < 1000, для которого 1/d имеет самый длинный период в десятичной записи.

**Общий алгоритм:**  
1. **Игнорировать непериодические дроби:** Для знаменателей, содержащих только множители 2 и 5, дробь конечная (период отсутствует).  
2. **Вычислить длину периода:** 
   - Нужно найти минимальное k, такое что 10ᵏ при делении на d даёт остаток 1;
   - Это число k и будет длиной периода десятичной дроби;
   - Простой пример: для d = 7: 10¹ = 10 (остаток 3), 10² = 100 (остаток 2), ..., 10⁶ = 1000000 (остаток 1), значит, длина периода = 6.
3. **Найти максимум:** Перебрать d от 2 до 999, вычислить длину периода и выбрать d с наибольшей длиной.

**Ожидаемый ответ:**  
983

## Реализации решения задачи 5

### 1.1. Хвостовая рекурсия
```clojure
; Алгоритм Евклида для НОД (наибольшего общего делителя двух чисел)
(defn gcd [a b] 
  (if (zero? b)
    a                      ; THEN - возврат результата
    (recur b (mod a b))))  ; ELSE - хвостовой вызов с новыми параметрами (оптимизированный выбор с recur, компиляция в цикл без накопления стека)
                           ; В рекурсивном вызове заменяем a на b, b на остаток от деления a на b 

; Вычисление НОК (наименьшего общего кратного двух чисел)
(defn lcm [a b] 
  (if (or (zero? a) (zero? b)) 
    0
    (/ (* a b) (gcd a b))))

(defn smallest-multiple-tail [n]
  (let [nums (range 2 (inc n))]   ; LET - связывание переменных
    (loop [remaining nums         ; LOOP - начало цикла с начальными значениями
           result 1]
      (if (empty? remaining)      
        result
        (recur (rest remaining)   ; rest - вощвращает все, кроме первого элемента
               (lcm result (first remaining)))))))
```

### 1.2. Обычная рекурсия
```clojure
(defn gcd-rec [a b]
  (if (zero? b)
    a
    (gcd-rec b (mod a b))))

(defn lcm-rec [a b]
  (/ (* a b) (gcd-rec a b)))

(defn smallest-multiple-rec [n]
  (letfn [(helper [nums acc]      ; LETFN - локальное определение функции
            (if (empty? nums)
              acc
              (helper (rest nums) 
                      (lcm-rec acc (first nums)))))]
    (helper (range 2 (inc n)) 1)))
```

### 2. Модульная версия
```clojure
(defn generate-sequence-sm [n]
  (range 2 (inc n)))                  ; от 2 до n включительно

(defn filter-numbers [numbers]
  (filter (fn [x] (> x 1)) numbers))  ; демонстрация фильтрации (для задачи избыточно)

(defn calculate-result [numbers]
  (reduce lcm 1 numbers))

(defn smallest-multiple-modular [n]
  (-> n
      generate-sequence-sm            ; этап 1: генерация
      filter-numbers                  ; этап 2: фильтрация  
      calculate-result))              ; этап 3: свертка
```

### 3. Версия с отображением (map) и редукцией
```clojure
(defn prime-factors [n]
  (loop [num n
         divisor 2                    ; текущий простой делитель
         factors []]                  ; пустой вектор для простых множителей
    (cond                             ; COND - множественное ветвление
      (= num 1) factors               ; условие выхода
      (zero? (mod num divisor)) (recur (/ num divisor) divisor (conj factors divisor))
      :else (recur num (inc divisor) factors))))

(defn smallest-multiple-map [n]
  (let [numbers (range 2 (inc n))
        all-factors (map prime-factors numbers)      ; map к каждому из чисел применяет поиск всех простых множителей
        ; factor-powers - map с простыми числами и их максимальными степенями
        factor-powers (reduce (fn [acc factors]                               ; reduce свертка коллекции в одно значение
                               (merge-with max acc (frequencies factors)))    ; frequencies - преобразует список в map вида множитель - количество
                             {}                                               ; merge-with объединение map с функцией разрешения конфликтов
                             all-factors)]
    (reduce-kv (fn [result factor power]                                      ; reduce-kv - это свертка по ключам и значениям map
                 (* result (reduce * (repeat power factor)))) 
               1
               factor-powers)))
```


### 4. Версия со спец. синтаксисом для циклов (for)
```clojure
(defn smallest-multiple-for [n]
  (let [numbers (range 2 (inc n))
        ;; for создает ленивую последовательность промежуточных результатов
        steps (for [i (range 1 (inc (count numbers)))]          ; count - подсчет количества элементов (+1 с инкрементом)
                (reduce lcm 1 (take i numbers)))]               ; Интересно, что можно передавать не число, а сразу несколько, НОК всё равно вычислится попарно
    (last steps)))
```

### 5. Версия с бесконечными списками
```clojure
(defn smallest-multiples-infinite []
  (let [all-numbers (iterate inc 2)                       ; создание бесконечной ленивой последовательности
        multiples-seq (reductions lcm 1 all-numbers)]     ; reductions запоминает промежуточные результаты
    multiples-seq))

(defn smallest-multiple-lazy-infinite [n]
  (->> (smallest-multiples-infinite)
       (take n)           ; Берем первые n элементов
       (last)))           ; Берем последний (для n=20)                    ; Берем последний элемент
```

### 6. Реализация на Java
```java
    public static long gcd(long a, long b) {
        if (b == 0) {
            return a;
        }
        return gcd(b, a % b);
    }
    
    public static long lcm(long a, long b) {
        return a * b / gcd(a, b);
    }

    // Основная функция для поиска искомого числа
    public static long smallestMultiple(int n) {
        long result = 1; 
        for (int i = 2; i <= n; i++) {
            result = lcm(result, i);
        }
        return result;
    }
```


## Реализации решения задачи 26

### Вспомогательная функция для вычисления длины цикла
```clojure
(defn cycle-length [n]
  (if (<= n 1)
    0                                                ; ← Возвращаем 0 вместо nil/исключения, цикл не существует
    (let [remainders (java.util.HashSet.)            ; для быстрого поиска повторяющихся остатков (создание пустого множества)
          remainders-list (java.util.ArrayList.)]    ; для сохранения порядка остатков (создание пустого списка)
      (loop [r 1]
        (let [r (mod (* r 10) n)]                    ; эмуляция деления в столбик, вычисление следующего остатка
          (cond
            (zero? r) 0
            (.contains remainders r) (let [idx (.indexOf remainders-list r)]       ; .contains - проверка наличия остатка в HashSet, .indexOf - поиск позиции в ArrayList
                                       (- (count remainders-list) idx))            ; (- (count remainders-list) idx) - вычисление длины цикла
            :else (do
                    (.add remainders r)
                    (.add remainders-list r)
                    (recur r))))))))
```

### 1.1. Хвостовая рекурсия
```clojure
(defn euler-26-tail-recursion [limit]
  (letfn [(find-max-cycle [n max-d max-len]
            (if (<= n 1) 
              max-d
              (let [len (cycle-length n)]
                (if (> len max-len)
                  (recur (dec n) n len)
                  (recur (dec n) max-d max-len)))))]
    (find-max-cycle (dec limit) 0 0)))
```

### 1.2. Обычная рекурсия 
```clojure
(defn euler-26-recursion [limit]
  (letfn [(helper [n max-d max-len]
            (if (<= n 1)  
              max-d
              (let [curr-len (cycle-length n)]
                (if (> curr-len max-len)
                  (helper (dec n) n curr-len)  
                  (helper (dec n) max-d max-len)))))] 
    (helper (dec limit) 0 0)))
```

### 2-3. Модульная реализация с map
```clojure
;; Генерация последовательности 
(defn generate-sequence-rc [limit]
  (range 2 limit))  ; от 2 до limit-1

;; Преобразование (map) - вычисление длины цикла для каждого числа
(defn map-cycle-lengths [numbers]
  (map (fn [d] {:d d :len (cycle-length d)}) numbers))

;; Свертка (reduce) - поиск максимального элемента  
(defn reduce-to-max [items]
  (reduce (fn [max-item item]
            (if (> (:len item) (:len max-item))
              item
              max-item))
          {:d 0 :len 0}
          items))

;; Извлечение результата
(defn extract-result [max-item]
  (:d max-item))

;; Основная функция
(defn euler-26-modular-map [limit]
  (-> limit
      generate-sequence-rc      ; генерация
      map-cycle-lengths         ; преобразование (map)  
      reduce-to-max             ; свертка (reduce)
      extract-result))          ; извлечение результата
```


### 4. Специальный синтаксис для циклов
```clojure
(defn euler-26-for [limit]
  (let [max-sequence (for [d (range 2 limit)                         ; list comprehension (генератор списков)
                           :let [len (cycle-length d)]]              ; длина цикла для каждого
                       {:d d :len len})                              ; map для каждого элемента
        max-item (reduce (fn [max-result item]
                           (if (> (:len item) (:len max-result))
                             item
                             max-result))
                         {:d 0 :len 0}
                         max-sequence)]
    (:d max-item)))
```

### 5. Работа с бесконечными списками
```clojure
(defn euler-26-infinite []
  (let [all-numbers (iterate inc 2)
        number-cycles (map (fn [d] {:d d :len (cycle-length d)}) all-numbers)
        max-sequence (reductions (fn [max-item item]
                                   (if (> (:len item) (:len max-item))
                                     item
                                     max-item))
                                 {:d 0 :len 0}
                                 number-cycles)]
    max-sequence))

(defn euler-26-lazy-infinite [limit]
  (->> (euler-26-infinite)
       (take (- limit 1))     
       (last)                 
       (:d)))
```

### 6. Реализация на Java
```java
 public static int cycleLength(int n) {
        if (n <= 1) {
            throw new IllegalArgumentException("n must be greater than 1");
        }
        
        int remainder = 1;
        java.util.Map<Integer, Integer> seen = new java.util.HashMap<>();
        int position = 0;
        
        while (remainder != 0) {
            if (seen.containsKey(remainder)) {
                return position - seen.get(remainder);
            }
            seen.put(remainder, position);
            remainder = (remainder * 10) % n;
            position++;
        }
        
        return 0; // Конечная десятичная дробь
    }
    
    public static int longestRecurringCycle(int limit) {
        int maxD = 0;
        int maxLen = 0;
        
        for (int d = 2; d < limit; d++) {
            int len = cycleLength(d);
            if (len > maxLen) {
                maxLen = len;
                maxD = d;
            }
        }
        
        return maxD;
    }
```

## Тесты и результаты прохождения
```clojure
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
```
По итогам прохождения тестов:

### 5 задача

"Elapsed time: 0.0272 msecs"

"Elapsed time: 0.0585 msecs"

"Elapsed time: 0.0386 msecs"

"Elapsed time: 0.2284 msecs"

"Elapsed time: 0.1485 msecs"

"Elapsed time: 0.0464 msecs"


### 26 задача

"Elapsed time: 14.9995 msecs"

"Elapsed time: 12.8545 msecs"

"Elapsed time: 14.2968 msecs"

"Elapsed time: 10.7554 msecs"

"Elapsed time: 10.785 msecs"

Ran 9 tests containing 60 assertions.
0 failures, 0 errors.




## Выводы

В ходе лабораторной работы алгоритмы решения 5 и 26 задач проекта Эйлер были реализованы с применением различных возможностей языка clojure. Для 5 задачи, если оценивать по производительности, хвостовая рекурсия оказалась наиболее эффективным методом, однако и другие реализации имеют свои преимущества: модульная версия наиболее наглядна и удобна для тестирования в REPL, реализация с циклом for позволяет отслеживать промежуточные результаты расчетов и в моем случае наиболее проста с точки зрения алгоритма. В 26 задаче по производительности выигрывает цикл с for и бесконечные списки. По реализации алгоритма для этой задачи все функции очень схожи, большая часть математической логики вынесена во вспомогательную функцию, поэтому в самих функциях происходит одно и то же. 

Из наблюдений: 

– Удобная работа с данными, их передачей в функции. Например, несмотря на то что у меня функция определена для 2 чисел, я могу передать число и вектор, что приведет к вычислению значения функции для всех поочередно пар чисел (результат предыдущего вычисления и новое число). В 5 задаче это позволило, к примеру, достаточно легко вычислять НОК сразу для всех рассматриваемых чисел. 

– В реализации вспомогательной функции для вычисления длины цикла в 26 задаче я использовала типы данных Java, они отлично поддерживаются и позволяют использовать ключевые слова для упрощения реализации. 

– В сравнении, например, с лямбда-выражениями в других языках программирования, анонимные функции, распространенные в clojure, намного более читаемы и просты для понимания кода.
