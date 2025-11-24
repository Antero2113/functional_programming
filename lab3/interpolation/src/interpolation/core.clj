(ns interpolation.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))


;; ============================================================================
;;                               АЛГОРИТМЫ
;; ============================================================================

(defn linear-interp [[x1 y1] [x2 y2] x]
  (if (= x1 x2)
    y1
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))

(defn divided-diffs [pts]
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
  (let [xs (map first pts)
        dif (divided-diffs pts)]
    (loop [i 1 acc (ffirst dif) w 1.0]
      (if (= i (count pts))
        acc
        (let [w' (* w (- x (nth xs (dec i))))
              acc' (+ acc (* (nth (nth dif i) 0) w'))]
          (recur (inc i) acc' w'))))))


;; ============================================================================
;;                       ГЕНЕРАЦИЯ ОБЛАСТЕЙ И ТОЧЕК
;; ============================================================================

(defn newton-range [window]
  [(ffirst window) (first (last window))])

(defn linear-range [window]
  (let [[p1 p2] (take-last 2 window)]
    [(first p1) (first p2)]))

(defn gen-xs [start end step last-x]
  (let [start (if last-x
                (max start (+ last-x step))
                start)]
    (when (< start end)
      (->> (range start (+ end step) step)
           (take-while #(<= % end))
           vec))))


;; ============================================================================
;;                       ОКНО И ПОТОКОВАЯ ЛОГИКА
;; ============================================================================

(defn window-size [alg n]
  (case alg
    :linear 2
    :newton n))

(defn push-window [window pt max-size]
  (let [w (conj window pt)]
    (if (> (count w) max-size)
      (vec (rest w))
      w)))

(defn intervals-of-window [window]
  (map (fn [[a b]] [(first a) (first b)]) (partition 2 1 window)))

(defn new-intervals
  "Возвращает интервалы, которые были в новом окне, но не были в предыдущем."
  [old-window new-window]
  (let [old (set (intervals-of-window old-window))
        new (set (intervals-of-window new-window))]
    (sort-by first (set/difference new old))))


;; ============================================================================
;;                      ВЫЧИСЛЕНИЕ ПО АЛГОРИТМАМ
;; ============================================================================

(defn compute-linear [window step last-x]
  (let [[a b] (linear-range window)]
    (gen-xs a b step last-x)))

(defn compute-newton [window step last-x]
  (let [[a b] (newton-range window)]
    (gen-xs a b step last-x)))

(defn process-alg
  "Возвращает карту {:alg alg :pts [[x y] ...] :last-x new-last-x}"
  [alg window step n last-x]
  (let [xs (case alg
             :linear (compute-linear window step last-x)
             :newton (compute-newton window step last-x))]
    (when (seq xs)
      {:alg alg
       :pts (mapv (fn [x]
                    [x (case alg
                         :linear (linear-interp (nth window (- (count window) 2))
                                                (last window) x)
                         :newton (newton-interp window x))])
                  xs)
       :last-x (last xs)})))


(defn process-final-alg [alg window step n last-x]
  (let [[a b] (case alg
                :linear (linear-range window)
                :newton (newton-range window))
        xs (gen-xs a b step last-x)]
    (mapv (fn [x]
            [x (case alg
                 :linear (if (= x b) (second (last window))
                             (linear-interp (nth window (- (count window) 2))
                                            (last window) x))
                 :newton (if (= x b) (second (last window))
                             (newton-interp window x)))] )
          xs)))


;; ============================================================================
;;                            ПАРСИНГ ВВОДА
;; ============================================================================

(defn parse-line [s]
  (try
    (let [[a b] (-> s str/trim (str/replace ";" " ") (str/replace "\t" " ") (str/split #"\s+"))]
      [(Double/parseDouble a) (Double/parseDouble b)])
    (catch Exception _ nil)))


;; ============================================================================
;;                             ОСНОВНОЙ ЦИКЛ
;; ============================================================================

(defn -main [& args]
  ;; ---- парсим аргументы ----------------------------------------------------
  (let [algos (->> args
                   (filter #(#{ "--linear" "--newton"} %))
                   (map {"--linear" :linear "--newton" :newton})
                   (into []))
        algos (if (empty? algos) [:linear] algos)
        step (if-let [i (.indexOf args "--step")]
               (Double/parseDouble (nth args (inc i)))
               1.0)
        n   (if-let [i (.indexOf args "-n")]
              (Integer/parseInt (nth args (inc i)))
              4)

        ;; максимальный размер окна, который нам нужен (для буфера n+1)
        max-window-size (apply max (map #(window-size % n) algos))]

    ;; ---- потоковая обработка ----------------------------------------------
    (loop [{:keys [buffer last] :or {buffer [] last {}} :as st} {:buffer [] :last {}}]
      (if-let [line (read-line)]
        (if-let [pt (parse-line line)]
          (let [buf2 (conj buffer pt)]
            ;; запускаем интерполяцию, когда в буфере стало max-window-size + 1
            (if (= (count buf2) (inc max-window-size))
              (let [window (vec (butlast buf2))   ;; окно = предыдущие max-window-size точек
                    ;; триггерная точка = last buf2 (не используется напрямую
                    ;; в вычислениях, но служит причиной запуска)
                    _trig   (last buf2)

                    ;; выполняем расчёт для каждого алгоритма (если окно нужного размера)
                    new-last
                    (reduce
                     (fn [m alg]
                       (let [req (window-size alg n)]
                         (if (= (count window) req)
                           (let [lr (get last alg)
                                 res (process-alg alg window step n lr)]
                             (if res
                               (do
                                 (doseq [[x y] (:pts res)]
                                   (println (name alg) ":" x y))
                                 (assoc m alg (:last-x res)))
                               m))
                           m)))
                     last
                     algos)]
                ;; удаляем самую старую точку из буфера (чтобы размер <= max-window-size)
                (recur {:buffer (vec (rest buf2))
                        :last   (merge last new-last)}))

              ;; иначе — просто копим точку
              (recur {:buffer buf2 :last last})))

          ;; строка не распарсилась — пропускаем
          (recur st))

        ;; ==================== EOF =====================
(do
  ;; при конец-вода делаем финальный сдвиг окна:
  ;; считаем, что пришла "фиктивная" новая точка,
  ;; но не используем её в расчёте — она лишь триггер.
  (when (>= (count buffer) max-window-size)
    (let [window (vec (take-last max-window-size buffer))]

      (doseq [alg algos]
        (let [req (window-size alg n)]
          (when (= (count window) req)

            ;; last-x = последняя посчитанная точка данного алгоритма
            (let [lr (get last alg)
                  res (process-final-alg alg window step n lr)]

              (doseq [[x y] res]
                (println (name alg) ":" x y))))))))

  (System/exit 0))))))
