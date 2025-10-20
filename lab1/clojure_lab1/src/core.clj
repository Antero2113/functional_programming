(ns core
  (:import EulerProblem))

;; 1.1. Хвостовая рекурсия
(defn gcd [a b] ; Алгоритм Евклида для НОД
  (if (zero? b)
    a
    (recur b (mod a b)))) 

(defn lcm [a b] ; НОК
  (if (or (zero? a) (zero? b))
    0
    (/ (* a b) (gcd a b))))

(defn smallest-multiple-tail [n]
  (let [nums (range 2 (inc n))]
    (loop [remaining nums
           result 1]
      (if (empty? remaining)
        result
        (recur (rest remaining)
               (lcm result (first remaining)))))))

;; 1.2. Обычная рекурсия
(defn gcd-rec [a b]
  (if (zero? b)
    a
    (gcd-rec b (mod a b))))

(defn lcm-rec [a b]
  (/ (* a b) (gcd-rec a b)))

(defn smallest-multiple-rec [n]
  (letfn [(helper [nums acc]
            (if (empty? nums)
              acc
              (helper (rest nums) 
                      (lcm-rec acc (first nums)))))]
    (helper (range 2 (inc n)) 1)))

; 2. Модульная версия с reduce
(defn generate-sequence-sm [n]
  (range 2 (inc n)))  ; от 2 до n включительно

(defn filter-numbers [numbers]
  numbers)  ; тривиальная фильтрация, можно оставить как есть

(defn calculate-result [numbers]
  (reduce lcm 1 numbers))

(defn smallest-multiple-modular [n]
  (-> n
      generate-sequence-sm    ; этап 1: генерация
      filter-numbers      ; этап 2: фильтрация  
      calculate-result))  ; этап 3: свертка

;; 3. Версия с отображением (map) и редукцией - ИСПРАВЛЕННАЯ
(defn prime-factors [n]
  (loop [num n
         divisor 2
         factors []]
    (cond
      (= num 1) factors
      (zero? (mod num divisor)) (recur (/ num divisor) divisor (conj factors divisor))
      :else (recur num (inc divisor) factors))))

(defn smallest-multiple-map [n]
  (let [numbers (range 2 (inc n))
        all-factors (map prime-factors numbers)
        factor-powers (reduce (fn [acc factors]
                               (merge-with max acc (frequencies factors)))
                             {}
                             all-factors)]
    ;; ИСПРАВЛЕНИЕ: правильное целочисленное возведение в степень
    (reduce-kv (fn [result factor power]
                 (* result (reduce * (repeat power factor)))) ; ← Исправлено
               1
               factor-powers)))

;; 4. Версия со спец. синтаксисом для циклов (for)
(defn smallest-multiple-for [n]
  (let [numbers (range 2 (inc n))
        ;; for создает ленивую последовательность промежуточных результатов
        steps (for [i (range 1 (inc (count numbers)))]
                (reduce lcm 1 (take i numbers)))]
    (last steps)))

;; 5. Работа с бесконечными списками - ИСПРАВЛЕННАЯ
(defn smallest-multiples-infinite []
  (let [all-numbers (iterate inc 2)
        multiples-seq (reductions lcm 1 all-numbers)]
    multiples-seq))

(defn smallest-multiple-lazy-infinite [n]
  (->> (smallest-multiples-infinite)
       (take n)           ; ← Исправлено: берем первые n элементов
       (last)))           ; ← Берем последний (для n=20)


;; 26 ЗАДАЧА. RECIPROCAL CYCLES

;; Вспомогательная функция для вычисления длины цикла 
(defn cycle-length [n]
  ;; ИСПРАВЛЕНИЕ: правильная проверка входных данных
  (if (<= n 1)
    0  ; ← Возвращаем 0 вместо nil/исключения
    (let [remainders (java.util.HashSet.)
          remainders-list (java.util.ArrayList.)]
      (loop [r 1]
        (let [r (mod (* r 10) n)]
          (cond
            (zero? r) 0
            (.contains remainders r) (let [idx (.indexOf remainders-list r)]
                                       (- (count remainders-list) idx))
            :else (do
                    (.add remainders r)
                    (.add remainders-list r)
                    (recur r))))))))

;; 1.1. Хвостовая рекурсия 
(defn euler-26-tail-recursion [limit]
  (letfn [(find-max-cycle [n max-d max-len]
            (if (<= n 1)  ; ИСПРАВЛЕНО: условие остановки
              max-d
              (let [len (cycle-length n)]
                (if (> len max-len)
                  (recur (dec n) n len)
                  (recur (dec n) max-d max-len)))))]
    (find-max-cycle (dec limit) 0 0)))

;; 1.2. Обычная рекурсия
(defn euler-26-recursion [limit]
  (letfn [(helper [n max-d max-len]
            (if (<= n 1)  ; ИСПРАВЛЕНО: условие остановки
              max-d
              (let [curr-len (cycle-length n)]
                (if (> curr-len max-len)
                  (helper (dec n) n curr-len)  
                  (helper (dec n) max-d max-len)))))] 
    (helper (dec limit) 0 0)))

;; 2-3. Модульная реализация с явным разделением на этапы

;; 1. Генерация последовательности - ИСПРАВЛЕННАЯ
(defn generate-sequence-rc [limit]
  (range 2 limit))  ; от 2 до limit-1

;; 2. Преобразование (map) - вычисление длины цикла для каждого числа
(defn map-cycle-lengths [numbers]
  (map (fn [d] {:d d :len (cycle-length d)}) numbers))

;; 3. Свертка (reduce) - поиск максимального элемента  
(defn reduce-to-max [items]
  (reduce (fn [max-item item]
            (if (> (:len item) (:len max-item))
              item
              max-item))
          {:d 0 :len 0}
          items))

;; 4. Извлечение результата
(defn extract-result [max-item]
  (:d max-item))

;; Основная функция
(defn euler-26-modular-map [limit]
  (-> limit
      generate-sequence-rc      ; генерация
      map-cycle-lengths     ; преобразование (map)  
      reduce-to-max         ; свертка (reduce)
      extract-result))      ; извлечение результата


;; 4. Специальный синтаксис для циклов 
(defn euler-26-loop [limit]
  (let [result (volatile! {:d 0 :len 0})]
    (doseq [d (range 2 limit)]  ; ИСПРАВЛЕНО: правильный диапазон
      (let [len (cycle-length d)]
        (when (> len (:len @result))
          (vswap! result assoc :d d :len len))))
    (:d @result)))

;; 5. Работа с бесконечными списками
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
       (take (- limit 1))     ; ← Исправлено: берем элементы для d < limit
       (last)                 ; ← Берем последний элемент
       (:d)))


(defn -main
  "Project Euler Problem 5 and 26"
  [& args]
  (println "Smallest positive number evenly divisible by all numbers from 1 to 20:")

   (let [java-result (EulerProblem/smallestMultiple 20)]
    
    (println "Java result:   " java-result))


  (let [result-tail (smallest-multiple-tail 20)
        result-rec (smallest-multiple-rec 20)
        result-modular (smallest-multiple-modular 20)
        result-map (smallest-multiple-map 20)
        result-for (smallest-multiple-for 20)
        result-lazy (smallest-multiple-lazy-infinite 20)]
    
    (println "1.1. Хвостовая рекурсия:        " result-tail)
    (println "1.2. Обычная рекурсия:          " result-rec)
    (println "2. Модульная версия (reduce): " result-modular)
    (println "3. С отображением (map):      " result-map)
    (println "4. Со спец. синтаксисом (for):" result-for)
    (println "5. Ленивые списки:            " result-lazy)
    
    (println "\nВсе результаты совпадают:" 
             (apply = [result-tail result-rec result-modular result-map 
                      result-for result-lazy])))

  (println "Наибольшая длина цикла в десятичном представлении дроби 1/d:")

   (let [java-result (EulerProblem/longestRecurringCycle 1000)]
    
    (println "Java result:   " java-result))
                      
  (let [result-tail (euler-26-tail-recursion 1000)
      result-rec (euler-26-recursion 1000)
      result-modular (euler-26-modular-map 1000)
      result-loop (euler-26-loop 1000)
      result-lazy (euler-26-lazy-infinite 1000)]
  
  (println "1.1. Хвостовая рекурсия:        " result-tail)
  (println "1.2. Обычная рекурсия:          " result-rec)
  (println "2-3. Модульная версия (reduce) с map:   " result-modular)
  (println "4. Со спец. синтаксисом (loop): " result-loop)
  (println "5. Ленивые списки:              " result-lazy)))
