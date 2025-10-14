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
(defn generate-sequence [n]
  (range 2 (inc n)))

(defn filter-numbers [numbers]
  numbers)

(defn calculate-result [numbers]
  (reduce lcm 1 numbers))

(defn smallest-multiple-modular [n]
  (-> n
      generate-sequence    ; этап 1: генерация
      filter-numbers      ; этап 2: фильтрация
      calculate-result))  ; этап 3: свертка

;; 3. Версия с отображением (map) и редукцией
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
        ;; Используем map для вычисления простых множителей каждого числа
        all-factors (map prime-factors numbers)
        ;; Группируем множители по максимальным степеням
        factor-powers (reduce (fn [acc factors]
                               (merge-with max acc (frequencies factors)))
                             {}
                             all-factors)]
    ;; Вычисляем произведение
    (reduce-kv (fn [result factor power]
                 (* result (long (Math/pow factor power))))
               1
               factor-powers)))

;; 4. Версия со спец. синтаксисом для циклов (for)
(defn smallest-multiple-for [n]
  (let [numbers (range 2 (inc n))
        ;; for создает ленивую последовательность промежуточных результатов
        steps (for [i (range 1 (inc (count numbers)))]
                (reduce lcm 1 (take i numbers)))]
    (last steps)))

;; 5. Работа с бесконечными списками
(defn smallest-multiples-infinite []
  (let [all-numbers (iterate inc 2)
        multiples-seq (reductions lcm 1 all-numbers)]
    multiples-seq))  

;; Функция для получения НОК чисел от 2 до n
(defn smallest-multiple-lazy-infinite [n]
  (->> (smallest-multiples-infinite)
       (drop (- n 1))   ; Пропускаем до нужной позиции (для n=5 берем 4-й элемент)
       (first)))


;; 26 ЗАДАЧА. RECIPROCAL CYCLES

;; Вспомогательная функция для вычисления длины цикла
(defn cycle-length [n]
  (when (<= n 1)
    (throw (IllegalArgumentException. "n must be greater than 1")))
  (loop [remainder 1
         seen {}
         position 0]
    (cond
      (zero? remainder) 0  ; Конечная десятичная дробь
      (contains? seen remainder) (- position (seen remainder))  ; Найден цикл
      :else (recur (rem (* remainder 10) n)
                   (assoc seen remainder position)
                   (inc position)))))

;; 1.1. Хвостовая рекурсия
(defn euler-26-tail-recursion [limit]
  (letfn [(find-max-cycle [n max-d max-len]
            (if (< n 2)
              max-d
              (let [len (cycle-length n)]
                (if (> len max-len)
                  (recur (dec n) n len)
                  (recur (dec n) max-d max-len)))))]
    (find-max-cycle (dec limit) 0 0)))

;; 1.2. Обычная рекурсия 
(defn euler-26-recursion [limit]
  (letfn [(helper [n max-d max-len]
            (if (< n 2)
              max-d
              (let [curr-len (cycle-length n)]
                (if (> curr-len max-len)
                  (helper (dec n) n curr-len)  
                  (helper (dec n) max-d max-len)))))] 
    (helper (dec limit) 0 0)))

;; 2-3. Модульная реализация и map 
(defn euler-26-modular-map [limit]
  (->> (range 2 limit)
       (map (fn [d] {:d d :len (cycle-length d)}))
       (reduce (fn [max-item item]
                 (if (> (:len item) (:len max-item))
                   item
                   max-item))
               {:d 0 :len 0})
       :d))

;; 4. Специальный синтаксис для циклов
(defn euler-26-loop [limit]
  (let [result (volatile! {:d 0 :len 0})] ; Изменяемое состояние через volatile!
    (doseq [d (range (dec limit) 1 -1)]  ; Специальный синтаксис цикла
      (let [len (cycle-length d)]
        (when (> len (:len @result))
          (vswap! result assoc :d d :len len))))
    (:d @result)))

;; 5. Работа с бесконечными списками
(defn euler-26-infinite []
  (let [all-numbers (iterate inc 2)
        number-cycles (map (fn [d] [d (cycle-length d)]) all-numbers)
        
        max-sequence (reductions (fn [current-max [d len]]
                                   (if (> len (second current-max))
                                     [d len]
                                     current-max))
                                 [0 0]     ; Начальное значение
                                 number-cycles)]
    max-sequence)) 

;; Функция для получения результата для первых N чисел
(defn euler-26-lazy-infinite [n]
  (->> (euler-26-infinite)
       (drop n)           ; Пропускаем первые n элементов
       (first)            ; Берем следующий элемент (n+1)
       (first)))          ; Извлекаем число d


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
