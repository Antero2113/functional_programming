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

(defn is-sorted? [buffer new-point]
  "Проверяет, что новая точка не нарушает сортировку по x"
  (if (empty? buffer)
    true
    (let [last-x (first (last buffer))
          new-x (first new-point)]
      (>= new-x last-x))))

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
   :cleanup-buffer (fn [buffer last-x]
                     "Удаляет уже обработанные точки, оставляя только последнюю точку окна"
                     (if last-x
                       (let [window (take-last 2 buffer)
                             x2 (first (second window))]
                         (if (>= last-x x2)
                           (vec (drop-while #(< (first %) x2) buffer))
                           buffer))
                       buffer))
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
                        [[x2 y2]]
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
  (let [[a b] (newton-range window)
        effective-last-x (if (and last-x (>= last-x a) (<= last-x b))
                           last-x
                           nil)]
    (gen-xs a b step effective-last-x)))

(defn create-newton-algorithm [n]
  "Создает алгоритм интерполяции Ньютона"
  {:window-size (constantly n)
   :can-process? #(>= (count %) (inc n))
   :get-window #(vec (take-last n (butlast %)))
   :cleanup-buffer (fn [buffer last-x]
                     "Удаляет уже обработанные точки, сохраняя минимум n+1 точек для работы алгоритма"
                     (if last-x
                       (let [min-required (inc n)
                             buffer-count (count buffer)]
                         (if (>= buffer-count min-required)
                           (let [window (take-last n (butlast buffer))
                                 window-start-x (first (first window))
                                 window-end-x (first (last window))]
                             (if (>= last-x window-end-x)
                               (let [cleaned (vec (drop-while #(< (first %) window-start-x) buffer))]
                                 (if (>= (count cleaned) min-required)
                                   cleaned
                                   (vec (take-last min-required buffer))))
                               buffer))
                           buffer))
                       buffer))
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
                        [[x-end (newton-interp window x-end)]]
                        (let [intermediate-xs (->> (iterate #(+ % step) start-x)
                                                   (take-while #(< % x-end))
                                                   vec)
                              all-xs (vec (concat intermediate-xs [x-end]))]
                          (mapv (fn [x] [x (newton-interp window x)]) all-xs)))))
   :algorithm-name (constantly "newton")})

;; Обработка потока данных

(defn process-algorithm
  "Обрабатывает один алгоритм интерполяции для текущего буфера.
   Возвращает {:last-x x :cleaned-buffer buffer} или nil"
  [algorithm buffer step last-x]
  (when ((:can-process? algorithm) buffer)
    (let [window ((:get-window algorithm) buffer)
          result ((:process algorithm) algorithm window step last-x)]
      (when result
        (doseq [[x y] (:pts result)]
          (println (str ((:algorithm-name algorithm)) ":") x y))
        (let [new-last-x (:last-x result)
              cleaned-buffer ((:cleanup-buffer algorithm) buffer new-last-x)]
          {:last-x new-last-x
           :cleaned-buffer cleaned-buffer})))))

(defn process-algorithms-final
  "Финальная обработка всех алгоритмов при EOF"
  [algorithms buffer step last-states]
  (doseq [algorithm algorithms]
    (let [algo-name ((:algorithm-name algorithm))
          window-size ((:window-size algorithm))]
      (when (>= (count buffer) window-size)
        (let [window (if (= algo-name "newton")
                       (vec (take-last window-size buffer))
                       ((:get-window algorithm) buffer))
              last-x (get last-states algo-name)
              result ((:process-final algorithm) algorithm window step last-x)]
          (doseq [[x y] result]
            (println (str algo-name ":") x y)))))))

(defn process-stream
  "Основная функция обработки потока данных"
  [algorithms step]
  (loop [buffer []
         last-states {}]
    (if-let [line (read-line)]
      (if-let [pt (parse-line line)]
        (if (is-sorted? buffer pt)
          (let [new-buffer (conj buffer pt)
                {updated-states :states
                 results :results}
                (reduce (fn [acc algorithm]
                          (let [algo-name ((:algorithm-name algorithm))
                                result (process-algorithm algorithm new-buffer step (get (:states acc) algo-name))]
                            (if result
                              {:states (assoc (:states acc) algo-name (:last-x result))
                               :results (conj (:results acc) result)}
                              {:states (:states acc)
                               :results (:results acc)})))
                        {:states last-states :results []}
                        algorithms)
                cleaned-buffer (if (seq results)
                                 (->> results
                                      (map :cleaned-buffer)
                                      (reduce (fn [min-buf buf]
                                                (if (< (count buf) (count min-buf))
                                                  buf
                                                  min-buf))
                                              new-buffer))
                                 new-buffer)]
            (recur cleaned-buffer updated-states))
          ;; Точка нарушает сортировку - выводим сообщение и пропускаем
          (do
            (println "ERROR: Point violates x sorting order. It'll be skipped.")
            (recur buffer last-states)))
        ;; Строка не распарсилась
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

