(ns interpolation.core
  (:require [clojure.string :as str])
  (:gen-class))

;; Парсинг и валидация входных данных

(defn parse-line [s]
  (let [s (-> s str/trim (str/replace #"[;\t]" " "))
        p (str/split s #"\s+")]
    (when (= 2 (count p))
      (try
        [(Double/parseDouble (p 0))
         (Double/parseDouble (p 1))]
        (catch Exception _ nil)))))

(defn xs-between [a b step last-x]
  (let [start (if last-x (max a (+ last-x step)) a)]
    (take-while #(<= % b)
                (iterate #(+ % step) start))))

(defn valid-x? [buffer [x _]]                
  (or (empty? buffer)
      (>= x (first (last buffer)))))

;; Линейная интерполяция

(defn linear-interp [[[x1 y1] [x2 y2]] x]
  (if (= x1 x2)
    y1
    (+ y1 (* (- y2 y1)
             (/ (- x x1) (- x2 x1))))))

(defn process-linear [buffer step last-x]
  (when (>= (count buffer) 2)
    (let [[p1 p2] (take-last 2 buffer)
          [x1 _] p1
          [x2 _] p2
          xs (xs-between x1 x2 step last-x)]
      (when (seq xs)
        {:last-x (last xs)
         :out (map (fn [x] [x (linear-interp [p1 p2] x)]) xs)}))))

;; Интерполяция Ньютона

(defn divided-diffs [pts]
  (let [xs (mapv first pts)]
    (loop [k 1
           acc [(mapv second pts)]]
      (if (= k (count pts))
        acc
        (recur
          (inc k)
          (conj acc
                (mapv
                  (fn [i]
                    (/ (- (get-in acc [(dec k) (inc i)])
                          (get-in acc [(dec k) i]))
                       (- (xs (+ i k))
                          (xs i))))
                  (range (- (count pts) k)))))))))

(defn newton-fn [pts]
  (let [xs  (mapv first pts)
        dif (divided-diffs pts)]
    (fn [x]
      (reduce
        (fn [acc i]
          (+ (get-in dif [i 0])
             (* (- x (xs i)) acc)))
        (last (map first dif))
        (range (dec (count pts)) -1 -1)))))

(defn process-newton [buffer n step last-x]
  ;; считаем только при наличии (n + 1)-й точки
  (when (>= (count buffer) (inc n))
    (let [window (take n (butlast buffer))
          [a _] (first window)
          [b _] (last window)
          f  (newton-fn window)
          xs (xs-between a b step last-x)]
      (when (seq xs)
        {:last-x (last xs)
         :out (map (fn [x] [x (f x)]) xs)}))))

(defn process-stream [use-linear? use-newton? n step]
  (loop [buffer []
         last-linear-x nil
         last-newton-x nil]
    (if-let [line (read-line)]
      (if-let [pt (parse-line line)]
        (if (valid-x? buffer pt)                    
          (let [buffer' (conj buffer pt)

                linear-res (when use-linear?
                             (process-linear buffer' step last-linear-x))

                newton-res (when use-newton?
                             (process-newton buffer' n step last-newton-x))]

            ;; вывод
            (when linear-res
              (doseq [[x y] (:out linear-res)]
                (println "linear :" x y)))

            (when newton-res
              (doseq [[x y] (:out newton-res)]
                (println "newton :" x y)))

            ;; ограничиваем буфер
            (let [max-size (max 2 (inc n))
                  buffer'' (vec (take-last max-size buffer'))]
              (recur buffer''
                     (or (:last-x linear-res) last-linear-x)
                     (or (:last-x newton-res) last-newton-x))))

          ;; x нарушает сортировку                  
          (do
            (binding [*out* *err*]
              (println "ERROR: x must be non-decreasing, skipped:" pt))
            (recur buffer last-linear-x last-newton-x)))

        ;; строка не распарсилась
        (recur buffer last-linear-x last-newton-x))

      ;; EOF
      (do
        (when use-linear?
          (when-let [res (process-linear buffer step last-linear-x)]
            (doseq [[x y] (:out res)]
              (println "linear :" x y))))

        (when use-newton?
          (when-let [res (process-newton buffer n step last-newton-x)]
            (doseq [[x y] (:out res)]
              (println "newton :" x y))))

        (System/exit 0)))))


(defn parse-args [args]
  {:linear? (some #{"--linear"} args)
   :newton? (some #{"--newton"} args)
   :step    (or (some->> args
                         (drop-while #(not= % "--step"))
                         second
                         Double/parseDouble)
                1.0)
   :n       (or (some->> args
                         (drop-while #(not= % "-n"))
                         second
                         Integer/parseInt)
                4)})

(defn -main [& args]
  (let [{:keys [linear? newton? n step]} (parse-args args)
        linear? (or linear? (not newton?))]
    (process-stream linear? newton? n step)))
