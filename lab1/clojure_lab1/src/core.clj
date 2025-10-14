(ns core
  (:import EulerProblem))

;; 1.1. Хвостовая рекурсия
(defn gcd-tail [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm-tail [a b]
  (/ (* a b) (gcd-tail a b)))

(defn smallest-multiple-tail [n]
  (let [nums (range 2 (inc n))]
    (loop [remaining nums
           result 1]
      (if (empty? remaining)
        result
        (recur (rest remaining)
               (lcm-tail result (first remaining)))))))

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
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (/ (* a b) (gcd a b))))

(defn smallest-multiple-modular [n]
  (->> (range 2 (inc n))     ; Генерация последовательности
       (reduce lcm 1)))      ; Свёртка с вычислением НОК


;; 3. Версия с отображением (map) и редукцией
(defn smallest-multiple-map [n]
  (let [numbers (range 2 (inc n))
        lcm-pairs (map (fn [x] [x x]) numbers)] ; Создаем пары [a b] для LCM
    (reduce (fn [acc [a b]] (lcm acc a)) 1 lcm-pairs)))

;; 4. Версия со спец. синтаксисом для циклов (for, doseq)
(defn smallest-multiple-for [n]
  (let [numbers (range 2 (inc n))]
    (loop [result 1
           [x & xs] numbers]
      (if (nil? x)
        result
        (recur (lcm result x) xs)))))


;; 5. Версия с шагом = НОК предыдущих чисел (самая эффективная)
(defn smallest-multiple-lazy-step [n]
  (let [numbers (range 2 (inc n))]
    (->> (reductions lcm 1 numbers)    ; Последовательность НОК: 1, lcm(1,2), lcm(lcm(1,2),3)...
         (last))))                     ; Берем последний элемент


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

;; 1.2. Обычная рекурсия (безопасная версия)
(defn euler-26-recursion [limit]
  (letfn [(helper [n max-d max-len]
            (if (< n 2)
              max-d
              (let [curr-len (cycle-length n)]
                (if (> curr-len max-len)
                  (recur (dec n) n curr-len)
                  (recur (dec n) max-d max-len)))))]
    (helper (dec limit) 0 0)))

;; 2. Модульная реализация
(defn euler-26-modular [limit]
  (->> (range 2 limit)
       (map (fn [d] {:d d :len (cycle-length d)}))
       (reduce (fn [max-item item]
                 (if (> (:len item) (:len max-item))
                   item
                   max-item))
               {:d 0 :len 0})
       :d))

;; 3. Генерация последовательности через map
(defn euler-26-map [limit]
  (let [cycles (map (fn [d] [d (cycle-length d)]) (range 2 limit))]
    (if (empty? cycles)
      0
      (->> cycles
           (apply max-key second)
           first))))

;; 4. Специальный синтаксис для циклов
(defn euler-26-loop [limit]
  (loop [d (dec limit)
         max-d 0
         max-len 0]
    (if (< d 2)
      max-d
      (let [len (cycle-length d)]
        (if (> len max-len)
          (recur (dec d) d len)
          (recur (dec d) max-d max-len))))))

;; 5. Работа с бесконечными списками
(defn euler-26-lazy [limit]
  (let [numbers (range 2 limit)
        cycles (map (fn [d] [d (cycle-length d)]) numbers)]
    (if (empty? cycles)
      0
      (->> cycles
           (apply max-key second)
           first))))



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
        result-lazy (smallest-multiple-lazy-step 20)]
    
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
      result-modular (euler-26-modular 1000)
      result-map (euler-26-map 1000)
      result-loop (euler-26-loop 1000)
      result-lazy (euler-26-lazy 1000)]
  
  (println "1.1. Хвостовая рекурсия:        " result-tail)
  (println "1.2. Обычная рекурсия:          " result-rec)
  (println "2. Модульная версия (reduce):   " result-modular)
  (println "3. С отображением (map):        " result-map)
  (println "4. Со спец. синтаксисом (loop): " result-loop)
  (println "5. Ленивые списки:              " result-lazy)))
