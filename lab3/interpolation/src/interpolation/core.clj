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
                        (cond
                          (and (some #{"--linear"} args) (some #{"--newton"} args)) nil  ; Если оба метода, размер окна должен быть задан явно
                          (some #{"--newton"} args) 4  ; Для newton по умолчанию 4
                          (some #{"--linear"} args) 2  ; Для linear по умолчанию 2
                          :else 2))]  ; По умолчанию 2
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

(defn divided-differences
  "Вычисляет разделенные разности для интерполяции Ньютона"
  [points]
  (let [n (count points)
        xs (map first points)
        ys (map second points)]
    (loop [diffs (vec ys)
           result [diffs]
           level 1]
      (if (>= level n)
        result
        (let [new-diffs (vec (map-indexed
                             (fn [i _]
                               (if (< i (- n level))
                                 (/ (- (nth diffs (inc i)) (nth diffs i))
                                    (- (nth xs (+ i level)) (nth xs i)))
                                 0))
                             diffs))]
          (recur new-diffs (conj result new-diffs) (inc level)))))))

(defn newton-interpolate
  "Интерполяция методом Ньютона для набора точек и значения x"
  [points x]
  (let [n (count points)
        xs (map first points)
        diffs (divided-differences points)
        first-diff (first (first diffs))]
    (loop [result first-diff
           product 1.0
           i 0]
      (if (>= i (dec n))
        result
        (let [new-product (* product (- x (nth xs i)))
              next-diff (nth (nth diffs (inc i)) 0)]
          (recur (+ result (* next-diff new-product))
                 new-product
                 (inc i)))))))

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
                     (if (and include-end? (<= start-x max-x))
                       ;; При EOF добавляем конечную точку, если она еще не была добавлена
                       (let [last-x (if (empty? result) start-x (first (last result)))
                             diff (- max-x last-x)
                             abs-diff (if (>= diff 0) diff (- diff))]
                         (if (and (> abs-diff epsilon) (<= start-x max-x))
                           (conj result [max-x (linear-interpolate point1 point2 max-x)])
                           result))
                       result)
                     (recur (+ x step) 
                            (conj result [x (linear-interpolate point1 point2 x)])))))]
    points))

(defn generate-newton-interpolation-points
  "Генерирует точки для интерполяции Ньютона с заданным шагом"
  [points step start-from-x include-end?]
  (let [[x1 _] (first points)
        [x2 _] (last points)
        [min-x max-x] (if (< x1 x2) [x1 x2] [x2 x1])
        start-x (max min-x start-from-x)
        epsilon 1e-10
        newton-points (loop [x start-x
                             result []]
                        (let [should-stop (if include-end?
                                           (> x (+ max-x epsilon))
                                           (>= x max-x))]
                          (if should-stop
                            (if (and include-end? (<= start-x max-x))
                              (let [last-x (if (empty? result) start-x (first (last result)))
                                    diff (- max-x last-x)
                                    abs-diff (if (>= diff 0) diff (- diff))]
                                (if (and (> abs-diff epsilon) (<= start-x max-x))
                                  (conj result [max-x (newton-interpolate points max-x)])
                                  result))
                              result)
                            (recur (+ x step)
                                   (conj result [x (newton-interpolate points x)])))))]
    newton-points))

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
                                      nil  ; При EOF, если next-x >= x2, обрабатываем в блоке ниже
                                      nil)  ; В обычном режиме останавливаемся перед x2
                                    next-x)))]
        (if (nil? start-from-x)
          (if (and is-last? (>= (count window) required-size))
            ;; При EOF генерируем все оставшиеся точки до x2 включительно
            (let [start-x (if last-output-x
                           (let [next-x (+ last-output-x step)]
                             (if (<= next-x x2) 
                               next-x
                               ;; Если следующая точка за пределами, проверяем нужно ли вывести x2
                               (if (< last-output-x x2) x2 nil)))
                           x1)]
              (if (nil? start-x)
                ;; Если start-x = nil, значит next-x > x2, выводим последнюю интерполированную точку еще раз
                (if last-output-x
                  [[[last-output-x (linear-interpolate p1 p2 last-output-x)] :linear]]
                  [])
                (let [interpolated (generate-interpolation-points p1 p2 step start-x true)]
                  (map (fn [point] [point :linear]) interpolated))))
            [])
          (let [interpolated (generate-interpolation-points p1 p2 step start-from-x is-last?)]
            (map (fn [point] [point :linear]) interpolated)))))))

(defn round-to-1-decimal
  "Округляет число до 1 знака после запятой"
  [n]
  (let [multiplied (* n 10.0)
        rounded (if (>= multiplied 0)
                  (long (+ multiplied 0.5))
                  (long (- multiplied 0.5)))
        result (/ rounded 10.0)]
    result))

(defn format-number
  "Форматирует число, округляя до 1 знака после запятой"
  [n]
  (let [rounded (round-to-1-decimal n)
        int-part (long rounded)
        fractional (* (- rounded int-part) 10.0)
        dec-abs (if (>= fractional 0) fractional (- fractional))
        dec-part (long (+ dec-abs 0.5))
        dec-str (str dec-part)]
    (str int-part "." dec-str)))

(defn format-output
  "Форматирует вывод результата интерполяции"
  [[[x y] algorithm]]
  (format "%s: %s %s" (name algorithm) 
          (format-number x) 
          (format-number y)))

(defn process-newton-interpolation
  "Обрабатывает интерполяцию Ньютона для текущего состояния"
  [state is-last?]
  (let [window (:window state)
        step (:step state)
        last-output-x (:last-output-x state nil)
        window-size (:window-size state)
        required-size (or window-size 4)
        ;; Для первого расчета нужно накопить required-size + 1 точек (5 для n=4)
        min-size-for-first (inc required-size)
        min-size (if (nil? last-output-x) min-size-for-first required-size)]
    (cond
      (< (count window) min-size) []
      :else
      (let [points (take-last required-size window)
            [x1 _] (first points)
            [x2 _] (last points)
            ;; Для метода Ньютона используем ту же логику, что и для линейной интерполяции
            start-from-x (cond
                          (nil? last-output-x) x1
                          (< last-output-x x1) (let [next-x (+ last-output-x step)]
                                                  (if (< next-x x1) x1 next-x))
                          :else (let [next-x (+ last-output-x step)]
                                  (if (>= next-x x2)
                                    (if is-last?
                                      nil
                                      nil)
                                    next-x)))]
        (if (nil? start-from-x)
          (if (and is-last? (>= (count window) required-size))
            ;; При EOF генерируем все оставшиеся точки до x2 включительно
            (let [start-x (if last-output-x
                           (let [next-x (+ last-output-x step)]
                             (if (<= next-x x2) 
                               next-x
                               (if (< last-output-x x2) x2 nil)))
                           x1)]
              (if (nil? start-x)
                ;; Если start-x = nil, значит next-x > x2, выводим последнюю интерполированную точку еще раз
                (if last-output-x
                  [[[last-output-x (newton-interpolate points last-output-x)] :newton]]
                  [])
                (let [interpolated (generate-newton-interpolation-points points step start-x true)]
                  (mapv (fn [point] [point :newton]) interpolated))))
            [])
          (let [interpolated (generate-newton-interpolation-points points step start-from-x is-last?)]
            (mapv (fn [point] [point :newton]) interpolated)))))))

(defn process-state
  "Обрабатывает состояние и выводит результаты интерполяции"
  [state is-last?]
  (let [max-x (reduce (fn [max-x algorithm]
                        (let [results (case algorithm
                                        :linear (process-linear-interpolation state is-last?)
                                        :newton (process-newton-interpolation state is-last?)
                                        [])
                              results-vec (vec results)]  ; Форсируем вычисление результатов
                          (reduce (fn [current-max result]
                                    (let [[[x y] _] result]
                                      (println (format-output result))
                                      (flush)  ; Принудительно сбрасываем буфер вывода
                                      (max (or current-max x) x)))
                                  max-x
                                  results-vec)))
                      (:last-output-x state)
                      (:algorithms state))
        final-max-x (or max-x (:last-output-x state))]
    (assoc state :last-output-x final-max-x)))

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
        required-size (or window-size 2)
        ;; Для метода Ньютона нужно накопить required-size + 1 для первого расчета
        max-window-size (if (and window-size (>= window-size 4))
                         (inc window-size)  ; Для метода Ньютона: n+1 для первого расчета
                         (max required-size 4))]  ; Минимум 4 для метода Ньютона
    (if (> (count sorted-window) max-window-size)
      (take-last max-window-size sorted-window)
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
      (let [min-window-size (if (some #(= % :newton) (:algorithms state))
                             (or (:window-size state) 4)
                             2)]
        (when (>= (count (:window state)) min-window-size)
          (process-fn state true)
          nil)))))  ; Возвращаем nil для завершения

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