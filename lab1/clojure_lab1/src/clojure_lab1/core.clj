(ns clojure-lab1.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (/ (Math/abs (* a b)) (gcd a b))))

(defn smallest-multiple [n]
  (reduce lcm (range 1 (inc n))))

(defn -main [& args]
  (let [result (smallest-multiple 20)]
    (println "Result:" result)
    
    ;; Простая проверка
    (println "Verification passed:" 
             (every? #(zero? (mod result %)) (range 1 21)))))
