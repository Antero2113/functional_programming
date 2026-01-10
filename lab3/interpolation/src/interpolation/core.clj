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

(defn valid-x? [buffer [x _]]
  (or (empty? buffer)
      (>= x (first (last buffer)))))

(defn xs-between [last-x a b step]
  (let [start (if last-x
                last-x
                0.0)]
    (take-while #(<= % b)
  (filter #(>= % a)
          (iterate #(+ % step) start)))
))


;; Линейная интерполяция

(defn linear-interp [[[x1 y1] [x2 y2]] x]
  (if (= x1 x2)
    y1
    (+ y1 (* (- y2 y1)
             (/ (- x x1) (- x2 x1))))))

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

;; Описание алгоритмов 

(def linear-algorithm
  {:id :linear
   :label "linear"
   :win-size 2
   :process
   (fn [buffer step last-x final?]
     (when (>= (count buffer) 2)
       (let [[p1 p2] (take-last 2 buffer)
             a (first p1)
             b (first p2)
             xs (xs-between last-x a b step)]
         (when (seq xs)
           {:out (map #(vector % (linear-interp [p1 p2] %)) xs)
            :last-x (last xs)}))))})


(defn newton-algorithm [n]
  {:id :newton
   :label "newton"
   :win-size (inc n)
   :process
   (fn [buffer step last-x final?]
     (when (>= (count buffer) (inc n))
       (let [window (if final?
                      (take-last n buffer)
                      (take n (butlast buffer)))
             [a _] (first window)
             [b _] (last window)
             f (newton-fn window)
             xs (xs-between last-x a b step)]
         (when (seq xs)
           {:out (map #(vector % (f %)) xs)
            :last-x (last xs)}))))})

;; Универсальная обработка алгоритма

(defn run-algorithm
  [alg buffer step last-x final?]
  (when-let [{:keys [out last-x]} ((:process alg) buffer step last-x final?)]
    (doseq [[x y] out]
      (println (:label alg) ":" x y))
    last-x))

;; Потоковая обработка входного потока

(defn process-stream [algorithms step]
  (loop [buffer []
         states (zipmap (map :id algorithms)
                        (repeat nil))]
    (if-let [line (read-line)]
      (if-let [pt (parse-line line)]
        (if (valid-x? buffer pt)
          (let [buffer' (conj buffer pt)
                max-win (apply max (map :win-size algorithms))
                buffer'' (vec (take-last max-win buffer'))

                states'
                (into {}
                      (for [alg algorithms]
                        (let [new-last-x (run-algorithm alg buffer'' step (states (:id alg)) false)]
                          [(:id alg) new-last-x])))]
            (recur buffer'' states'))
          (do
            (println "ERROR: x must be non-decreasing, skipped:" pt)
            (recur buffer states)))
        (recur buffer states))
      ;; EOF
      (doseq [alg algorithms]
        (run-algorithm alg buffer step (states (:id alg)) true)))))

;; Парсинг аргументов командной строки

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

(defn make-algorithms [{:keys [linear? newton? n]}]
  (cond-> []
    linear? (conj linear-algorithm)
    newton? (conj (newton-algorithm n))))

;; Точка входа

(defn -main [& args]
  (let [{:keys [linear? newton? step] :as opts} (parse-args args)
        linear? (or linear? (not newton?))
        algorithms (make-algorithms (assoc opts :linear? linear?))]
    (process-stream algorithms step)))
