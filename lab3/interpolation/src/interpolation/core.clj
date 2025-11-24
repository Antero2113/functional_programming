(ns interpolation.core
  (:require [clojure.string :as str]
            [clojure.set :as set])
  (:gen-class))


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



(defn linear-range [window]
  "Берем только последние две точки"
  (let [[p1 p2] (take-last 2 window)]
    [(first p1) (first p2)]))

(defn gen-xs [start end step last-x]
  (let [start (if last-x
                (max start (+ last-x step))
                start)]
    (when (< start end)
      (->> (iterate #(+ % step) start)
           (take-while #(<= % end))
           vec))))

(defn compute-linear [window step last-x]
  (let [[x1 x2] (linear-range window)]
    (gen-xs x1 x2 step last-x)))

(defn process-linear [window step last-x]
  (let [xs (compute-linear window step last-x)]
    (when (seq xs)
      {:pts (mapv (fn [x] [x (linear-interp (nth window (- (count window) 2))
                                            (last window) x)]) xs)
       :last-x (last xs)})))

(defn process-final-linear [window step last-x]
  "Финальный проход при EOF — гарантируем вывод последней точки"
  (let [[x1 y1] (first window)
        [x2 y2] (second window)
        xs (compute-linear window step last-x)]
    (vec
     (concat
       (mapv (fn [x] [x (linear-interp (first window) (second window) x)]) xs)
       (when (or (empty? xs) (< (last xs) x2))
         [[x2 y2]])))))



(defn newton-range [window]
  [(ffirst window) (first (last window))])

(defn compute-newton [window step last-x]
  (let [[a b] (newton-range window)]
    (gen-xs a b step last-x)))

(defn process-newton [window step last-x]
  (let [xs (compute-newton window step last-x)]
    (when (seq xs)
      {:pts (mapv (fn [x] [x (newton-interp window x)]) xs)
       :last-x (last xs)})))

(defn process-final-newton [window step last-x]
  (let [xs (compute-newton window step last-x)]
    (mapv (fn [x] [x (newton-interp window x)]) xs)))



(defn parse-line [s]
  (try
    (let [[a b] (-> s str/trim (str/replace ";" " ") (str/replace "\t" " ") (str/split #"\s+"))]
      [(Double/parseDouble a) (Double/parseDouble b)])
    (catch Exception _ nil)))


(defn -main [& args]
  (let [algos (->> args
                 (filter #(#{ "--linear" "--newton"} %))
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


    ;; потоковая обработка
   (loop [{:keys [buffer last] :or {buffer [] last {}}} {:buffer [] :last {}}]
  (if-let [line (read-line)]
    (if-let [pt (parse-line line)]
      (let [buf2 (conj buffer pt)
            ;; линейная интерполяция 
            last-linear
            (if (and (some #{:linear} algos) (>= (count buf2) 2))
              (let [window (take-last 2 buf2)
                    lr (get last :linear)
                    res (process-linear window step lr)]
                (when res
                  (doseq [[x y] (:pts res)]
                    (println "linear:" x y))
                  (:last-x res))))

            ;; Ньютон 
            last-newton
            (if (and (some #{:newton} algos) (>= (count buf2) (inc n)))
              (let [window (take-last n (butlast buf2))
                    lr (get last :newton)
                    res (process-newton window step lr)]
                (when res
                  (doseq [[x y] (:pts res)]
                    (println "newton:" x y))
                  (:last-x res))))]

        ;; продолжаем цикл с обновленным буфером и last
        (recur {:buffer buf2
                :last (cond-> last
                        last-linear (assoc :linear last-linear)
                        last-newton (assoc :newton last-newton))}))

      ;; строка не распарсилась
      (recur {:buffer buffer :last last}))

    ;; EOF
    (do
      ;; финальный проход линейной интерполяции
      (when (and (some #{:linear} algos) (>= (count buffer) 2))
        (let [window (take-last 2 buffer)
              lr (get last :linear)
              res (process-final-linear window step lr)]
          (doseq [[x y] res]
            (println "linear:" x y))))

      ;; финальный проход Ньютон
      (when (and (some #{:newton} algos) (>= (count buffer) n))
        (let [window (take-last n buffer)
              lr (get last :newton)
              res (process-final-newton window step lr)]
          (doseq [[x y] res]
            (println "newton:" x y))))

      (System/exit 0))))))

