(ns clojure-lab1.core)

;; Хвостовая рекурсия
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

;; Обычная рекурсия
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

; Модульная версия с reduce
(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn smallest-multiple-modular [n]
  (->> (range 2 (inc n))     ; Генерация последовательности
       (reduce lcm 1)))      ; Свёртка с вычислением НОК


(defn -main
  "Project Euler Problem 5 - Smallest multiple"
  [& args]
  (println "Smallest positive number evenly divisible by all numbers from 1 to 20:")
  (let [result-tail (smallest-multiple-tail 20)
        result-rec (smallest-multiple-rec 20)
        result-modular (smallest-multiple-modular 20)]
    (println "Хвостовая рекурсия:" result-tail)
    (println "Обычная рекурсия:  " result-rec)
    (println "Модульная версия:  " result-modular)
    (println "Все результаты совпадают:" (and (= result-tail result-rec)
                                             (= result-rec result-modular)))))
