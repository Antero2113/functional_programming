(ns interpolation.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.io BufferedReader]))

;; - window: скользящее окно точек (фиксированного размера)
;; - algorithms: список выбранных алгоритмов интерполяции
;; - step: шаг для генерации целевых точек
;; - window-size: размер окна для интерполяции
;; - last-output-x: последнее выведенное значение x (для избежания дублирования)
(defrecord ProgramState [window algorithms step window-size last-output-x])

(defn parse-args
  "Парсит аргументы командной строки"
  [args]
  (let [args-vec (vec args)
        get-arg-value (fn [key]
                        (when-let [idx (some #(when (= (nth args-vec % nil) key) %) 
                                             (range (count args-vec)))]
                          (nth args-vec (inc idx) nil)))
        step (or (get-arg-value "--step") "1.0")
        window-size (get-arg-value "-n")
        algorithms (cond-> []
                     (some #{"--linear"} args) (conj :linear)
                     (some #{"--newton"} args) (conj :newton))
        window-size (or (when window-size (Integer/parseInt window-size))
                        (if (some #{"--linear"} args) 2 nil))]
    {:algorithms (if (empty? algorithms) [:linear] algorithms)
     :step (Double/parseDouble step)
     :window-size window-size}))

(defn linear-interpolate
  "Линейная интерполяция между двумя точками [x1 y1] и [x2 y2] для значения x"
  [[x1 y1] [x2 y2] x]
  (if (= x1 x2)
    y1
    (let [y (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))]
      y)))

(defn generate-interpolation-points
  "Генерирует точки для интерполяции между двумя точками с заданным шагом"
  [point1 point2 step start-from-x include-end?]
  (let [[x1 y1] point1
        [x2 y2] point2
        [min-x max-x] (if (< x1 x2) [x1 x2] [x2 x1])
        start-x (max min-x start-from-x)
        epsilon 1e-10
        points (loop [x start-x
                      result []]
                 (let [should-stop (if include-end?
                                     ;; При EOF включаем точки до max-x включительно
                                     (> x (+ max-x epsilon))
                                     ;; В обычном режиме не включаем конечную точку
                                     (>= x max-x))]
                   (if should-stop
                     result
                     (recur (+ x step) 
                            (conj result [x (linear-interpolate point1 point2 x)])))))]
    points))

(defn get-window-size
  "Определяет размер окна для алгоритма"
  [algorithm window-size]
  (or window-size
      (case algorithm
        :linear 2
        :newton (or window-size 4)
        2)))

(defn process-linear-interpolation
  "Обрабатывает линейную интерполяцию для текущего состояния"
  [state is-last?]
  (let [window (:window state)
        step (:step state)
        last-output-x (:last-output-x state nil)
        required-size 2]
    (cond
      (< (count window) required-size) []
      :else
      (let [[p1 p2] (take-last required-size window)
            [x1 y1] p1
            [x2 y2] p2
            ;; Определяем начальную точку для генерации:
            ;; - Если last-output-x = nil, это первое окно - начинаем с x1
            ;; - Если last-output-x < x1, это новый сегмент после сдвига окна - продолжаем с шагом от last-output-x
            ;; - Если last-output-x >= x1, это продолжение - начинаем с last-output-x + step
            start-from-x (cond
                          (nil? last-output-x) x1
                          (< last-output-x x1) (let [next-x (+ last-output-x step)]
                                                  (if (< next-x x1) x1 next-x))  ; Сдвиг окна, продолжаем генерацию
                          :else (let [next-x (+ last-output-x step)]
                                  (if (>= next-x x2)
                                    (if is-last? 
                                      ;; При EOF, если next-x >= x2, проверяем, нужно ли вывести x2
                                      (if (<= last-output-x x2) x2 nil)
                                      nil)
                                    next-x)))]
        (if (nil? start-from-x)
          (if (and is-last? (>= (count window) required-size))
            ;; При EOF выводим последнюю точку из данных, если она еще не была выведена
            (if (or (nil? last-output-x) (< last-output-x x2))
              [[[x2 y2] :linear]]
              [])
            [])
          (let [interpolated (if (and is-last? (= start-from-x x2))
                             ;; Если start-from-x = x2 при EOF, выводим только эту точку
                             [[x2 y2]]
                             (generate-interpolation-points p1 p2 step start-from-x is-last?))]
            (map (fn [point] [point :linear]) interpolated)))))))

(defn format-number
  "Форматирует число, убирая лишние нули"
  [n]
  (let [s (str (double n))]
    (if (str/includes? s ".")
      (str/replace s #"\.?0+$" "")
      s)))

(defn format-output
  "Форматирует вывод результата интерполяции"
  [[[x y] algorithm]]
  (format "%s: %s %s" (name algorithm) 
          (format-number x) 
          (format-number y)))

(defn process-state
  "Обрабатывает состояние и выводит результаты интерполяции"
  [state is-last?]
  (let [max-x (reduce (fn [max-x algorithm]
                        (let [results (case algorithm
                                        :linear (process-linear-interpolation state is-last?)
                                        :newton [] ; TODO: реализовать позже
                                        [])]
                          (reduce (fn [current-max result]
                                    (let [[[x y] _] result]
                                      (println (format-output result))
                                      (flush)  ; Принудительно сбрасываем буфер вывода
                                      (max (or current-max x) x)))
                                  max-x
                                  results)))
                      (:last-output-x state)
                      (:algorithms state))]
    (assoc state :last-output-x max-x)))

(defn parse-line 
  "Парсит строку с координатами точки (форматы: x;y или x\ty или x y)"
  [line]
  (try
    (let [line (str/trim line)
          separator (cond
                      (str/includes? line ";") ";"
                      (str/includes? line "\t") "\t"
                      :else "\\s+")
          parts (str/split line (re-pattern separator))
          [x-str y-str] (take 2 parts)
          x (Double/parseDouble (str/trim x-str))
          y (Double/parseDouble (str/trim y-str))]
      [x y])
    (catch Exception _ nil)))

(defn read-next-point
  "Читает следующую точку из stdin (блокирующий вызов)"
  []
  (when-let [line (read-line)]
    (parse-line line)))

(defn update-window
  "Обновляет скользящее окно: добавляет новую точку и удаляет старые, если окно переполнено"
  [window new-point window-size]
  (let [sorted-window (sort-by first (conj window new-point))
        required-size (or window-size 2)]
    (if (> (count sorted-window) required-size)
      (take-last required-size sorted-window)
      sorted-window)))

(defn process-stream
  "Обрабатывает входной поток точек и применяет функцию обработки для каждого нового состояния"
  [initial-state process-fn]
  ;; Читаем точки по одной и сразу обрабатываем
  (loop [state (assoc initial-state :last-output-x nil)]
    (if-let [point (read-next-point)]
      (let [window-size (:window-size state)
            new-window (update-window (:window state) point window-size)
            new-state (assoc state :window new-window)
            ;; Проверяем, есть ли еще точки (неблокирующая проверка невозможна, поэтому считаем, что это не последняя)
            is-last? false
            updated-state (process-fn new-state is-last?)]
        (recur updated-state))
      ;; Обработка последнего сегмента при EOF
      (when (>= (count (:window state)) 2)
        (process-fn state true)))))

(defn -main
  "Interpolation clojure app"
  [& args]
  (let [config (parse-args args)
        initial-state (->ProgramState [] 
                                      (:algorithms config) 
                                      (:step config) 
                                      (:window-size config)
                                      nil)]
    (process-stream initial-state process-state)))