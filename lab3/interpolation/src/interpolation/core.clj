(ns interpolation.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn parse-line [s]
  "Парсит строку с координатами в форматах x;y, x\ty или x y"
  (try
    (let [[a b] (-> s str/trim 
                    (str/replace ";" " ") 
                    (str/replace "\t" " ") 
                    (str/split #"\s+"))]
      [(Double/parseDouble a) (Double/parseDouble b)])
    (catch Exception _ nil)))

(defn gen-xs [start end step last-x]
  "Генерирует последовательность точек x с заданным шагом"
  (let [start (if last-x
                (max start (+ last-x step))
                start)]
    (when (< start end)
      (->> (iterate #(+ % step) start)
           (take-while #(<= % end))
           vec))))

;; Линейная интерполяция

(defn linear-interp [[x1 y1] [x2 y2] x]
  "Вычисляет значение y для точки x между двумя заданными точками"
  (if (= x1 x2)
    y1
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))

(defn linear-range [window]
  "Определяет диапазон для линейной интерполяции (последние две точки)"
  (let [[p1 p2] (take-last 2 window)]
    [(first p1) (first p2)]))

(defn compute-linear-points [window step last-x]
  "Вычисляет точки для линейной интерполяции"
  (let [[x1 x2] (linear-range window)]
    (gen-xs x1 x2 step last-x)))

(defn create-linear-algorithm []
  "Создает алгоритм линейной интерполяции"
  {:window-size (constantly 2)
   :can-process? #(>= (count %) 2)
   :get-window #(vec (take-last 2 %))
   :process (fn [_ window step last-x]
              (let [xs (compute-linear-points window step last-x)]
                (when (seq xs)
                  {:pts (mapv (fn [x] [x (linear-interp (first window) (second window) x)]) xs)
                   :last-x (last xs)})))
   :process-final (fn [_ window step last-x]
                    (let [[x1 y1] (first window)
                          [x2 y2] (second window)
                          start-x (if last-x
                                    (max x1 (+ last-x step))
                                    x1)]
                      (if (>= start-x x2)
                        [[x2 y2]]  ; только последняя точка, если уже прошли весь диапазон
                        (let [intermediate-xs (->> (iterate #(+ % step) start-x)
                                                   (take-while #(< % x2))
                                                   vec)
                              all-xs (vec (concat intermediate-xs [x2]))]
                          (mapv (fn [x] [x (linear-interp [x1 y1] [x2 y2] x)]) all-xs)))))
   :algorithm-name (constantly "linear")})

;; Интерполяция методом Ньютона

(defn divided-diffs [pts]
  "Вычисляет разделенные разности для набора точек"
  (let [xs (map first pts)
        ys (map second pts)]
    (loop [level 0 acc [(vec ys)]]
      (if (= level (dec (count pts)))
        acc
        (recur (inc level)
               (conj acc
                     (vec (map (fn [a b x1 x0] (/ (- b a) (- x1 x0)))
                               (butlast (last acc))
                               (rest    (last acc))
                               (drop (inc level) xs)
                               (drop       level  xs)))))))))

(defn newton-interp [pts x]
  "Вычисляет значение полинома Ньютона для точки x"
  (let [xs (map first pts)
        dif (divided-diffs pts)]
    (loop [i 1 acc (ffirst dif) w 1.0]
      (if (= i (count pts))
        acc
        (let [w' (* w (- x (nth xs (dec i))))
              acc' (+ acc (* (nth (nth dif i) 0) w'))]
          (recur (inc i) acc' w'))))))

(defn newton-range [window]
  "Определяет диапазон для интерполяции Ньютона (от первой до последней точки)"
  [(ffirst window) (first (last window))])

(defn compute-newton-points [window step last-x]
  "Вычисляет точки для интерполяции Ньютона"
  (let [[a b] (newton-range window)]
    (gen-xs a b step last-x)))

(defn create-newton-algorithm [n]
  "Создает алгоритм интерполяции Ньютона"
  {:window-size (constantly n)
   :can-process? #(>= (count %) n)
   :get-window #(vec (take-last n %))
   :process (fn [_ window step last-x]
              (let [xs (compute-newton-points window step last-x)]
                (when (seq xs)
                  {:pts (mapv (fn [x] [x (newton-interp window x)]) xs)
                   :last-x (last xs)})))
   :process-final (fn [_ window step last-x]
                    (let [[x-start] (newton-range window)
                          x-end (first (last window))
                          start-x (if last-x
                                    (max x-start (+ last-x step))
                                    x-start)]
                      (if (>= start-x x-end)
                        [[x-end (newton-interp window x-end)]]  ; только последняя точка
                        (let [intermediate-xs (->> (iterate #(+ % step) start-x)
                                                   (take-while #(< % x-end))
                                                   vec)
                              all-xs (vec (concat intermediate-xs [x-end]))]
                          (mapv (fn [x] [x (newton-interp window x)]) all-xs)))))
   :algorithm-name (constantly "newton")})

;; Обработка потока данных

(defn process-algorithm
  "Обрабатывает один алгоритм интерполяции для текущего буфера"
  [algorithm buffer step last-x]
  (when ((:can-process? algorithm) buffer)
    (let [window ((:get-window algorithm) buffer)
          result ((:process algorithm) algorithm window step last-x)]
      (when result
        (doseq [[x y] (:pts result)]
          (println (str ((:algorithm-name algorithm)) ":") x y))
        (:last-x result)))))

(defn process-algorithms-final
  "Финальная обработка всех алгоритмов при EOF"
  [algorithms buffer step last-states]
  (doseq [algorithm algorithms]
    (when ((:can-process? algorithm) buffer)
      (let [window ((:get-window algorithm) buffer)
            last-x (get last-states ((:algorithm-name algorithm)))
            result ((:process-final algorithm) algorithm window step last-x)]
        (doseq [[x y] result]
          (println (str ((:algorithm-name algorithm)) ":") x y))))))

(defn process-stream
  "Основная функция обработки потока данных"
  [algorithms step]
  (loop [buffer []
         last-states {}]
    (if-let [line (read-line)]
      (if-let [pt (parse-line line)]
        (let [new-buffer (conj buffer pt)
              updated-states (reduce (fn [states algorithm]
                                       (let [algo-name ((:algorithm-name algorithm))
                                             last-x (process-algorithm algorithm new-buffer step (get states algo-name))]
                                         (if last-x
                                           (assoc states algo-name last-x)
                                           states)))
                                     last-states
                                     algorithms)]
          (recur new-buffer updated-states))
        (recur buffer last-states))
      (do
        (process-algorithms-final algorithms buffer step last-states)
        (System/exit 0)))))

;; Парсинг аргументов командной строки

(defn parse-args [args]
  "Парсит аргументы командной строки и возвращает конфигурацию"
  (let [algos (->> args
                   (filter #(#{"--linear" "--newton"} %))
                   (map {"--linear" :linear "--newton" :newton})
                   (into []))
        algos (if (empty? algos) [:linear] algos)
        
        step-idx (.indexOf args "--step")
        step (if (and (>= step-idx 0) (< (inc step-idx) (count args)))
               (Double/parseDouble (nth args (inc step-idx)))
               1.0)
        
        n-idx (.indexOf args "-n")
        n (if (and (>= n-idx 0) (< (inc n-idx) (count args)))
            (Integer/parseInt (nth args (inc n-idx)))
            4)]
    {:algorithms algos
     :step step
     :n n}))

(defn create-algorithms [config]
  "Создает список алгоритмов интерполяции на основе конфигурации"
  (let [{:keys [algorithms n]} config]
    (mapv (fn [algo-type]
            (case algo-type
              :linear (create-linear-algorithm)
              :newton (create-newton-algorithm n)))
          algorithms)))


(defn -main [& args]
  (let [config (parse-args args)
        algorithms (create-algorithms config)
        step (:step config)]
    (process-stream algorithms step)))

