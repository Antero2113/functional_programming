(ns interpolation.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

;; - points: накопленные точки (отсортированные по x)
;; - algorithm: выбранный алгоритм интерполяции
;; - step: шаг для генерации целевых точек
;; - window-size: размер окна для интерполяции
(defrecord ProgramState [points algorithm step window-size])

(defn linear-interpolation 
  "Функция линейной интерполяции"
  []
  ())

(defn parse-line 
  "Обрабатывает строку, введенную пользователем"
  [line]
  (try
    (let [separator (if (str/includes? line ";") ";" "\t")
          parts (str/split line (re-pattern separator))
          [x-str y-str] (take 2 parts)
          x (Double/parseDouble (str/trim x-str))
          y (Double/parseDouble (str/trim y-str))]
      [x y])
    (catch Exception e nil)))

(defn read-points 
  "Читает ввод и возвращает ленивую последовательность строк, применяет функцию парсинга строки"
  []
  (->> (line-seq (java.io/BufferedReader. *in*))
       (keep parse-line)))

(defn add-point
  "Добавляет точку в коллекцию и сортирует по возрастанию x"
  [points new-point]
  (sort-by first (conj points new-point)))

(defn process-stream
  "Обрабатывает входной поток точек и применяет функцию обработки для каждого нового состояния"
  [initial-state process-fn]
  (let [points-seq (read-points)]
    ;; Точка рекурсии
    (loop [state initial-state
           points points-seq]
      ;; Выполняется только если points не пустая посл-ть, иначе выход из цикла
      (when-let [point (first points)]
        (let [new-state (update state :points add-point point)
              result (process-fn new-state)]
          (recur new-state (rest points)))))))

(defn -main
  "Interpolation clojure app"
  [& args]
  
  
  (println "Ожидание входных данных...")
  (process-stream initial-state process-fn)
  (println "Обработка завершена."))
