(ns clojure-lab1.core)

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
  (/ (* a b) (gcd a b)))

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

(defn -main
  "Project Euler Problem 5 - Smallest multiple"
  [& args]
  (println "Smallest positive number evenly divisible by all numbers from 1 to 20:")
  (let [result-tail (smallest-multiple-tail 20)
        result-rec (smallest-multiple-rec 20)
        result-modular (smallest-multiple-modular 20)
        result-map (smallest-multiple-map 20)
        result-for (smallest-multiple-for 20)
        result-lazy (smallest-multiple-lazy-step 20)
        ]
    
    (println "1.1. Хвостовая рекурсия:        " result-tail)
    (println "1.2. Обычная рекурсия:          " result-rec)
    (println "2. Модульная версия (reduce): " result-modular)
    (println "3. С отображением (map):      " result-map)
    (println "4. Со спец. синтаксисом (for):" result-for)
    (println "5. Ленивые списки:            " result-lazy)
    
    (println "\nВсе результаты совпадают:" 
             (apply = [result-tail result-rec result-modular result-map 
                      result-for result-lazy]))))
