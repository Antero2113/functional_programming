(ns sc-bag.core
  "Реализация Bag (multiset) на основе Separate Chaining Hashmap")

;; === Внутреннее представление ===

(defrecord Node [key count next])
(defrecord SCBag [buckets size hash-fn eq-fn load-factor])

;; === Вспомогательные функции ===

(defn- hash-bucket [key num-buckets hash-fn]
  (-> (hash-fn key)
      (mod num-buckets)))

(defn- default-hash [x]
  (hash x))

(defn- default-eq [a b]
  (= a b))

(defn- new-buckets [capacity]
  (vec (repeat capacity nil)))

(defn- ensure-capacity [sc-bag]
  (let [current-capacity (count (:buckets sc-bag))
        current-size (:size sc-bag)
        load-factor (:load-factor sc-bag)]
    (if (and (> current-capacity 0) 
             (>= (/ current-size current-capacity) load-factor))
      (let [new-capacity (* 2 current-capacity)
            new-buckets (atom (new-buckets new-capacity))
            hash-fn (:hash-fn sc-bag)]
        
        ;; Перехешируем все элементы
        (doseq [bucket (:buckets sc-bag)]
          (loop [current bucket]
            (when current
              (let [bucket-idx (hash-bucket (:key current) new-capacity hash-fn)
                    existing (get @new-buckets bucket-idx)
                    new-node (Node. (:key current) (:count current) existing)]
                (swap! new-buckets assoc bucket-idx new-node)
                (recur (:next current))))))
        
        (->SCBag @new-buckets current-size hash-fn (:eq-fn sc-bag) load-factor))
      sc-bag)))

(defn- update-bucket [bucket key count-update eq-fn]
  (let [new-count (count-update (loop [current bucket]
                                  (if current
                                    (if (eq-fn (:key current) key)
                                      (:count current)
                                      (recur (:next current)))
                                    0)))]
    (if (pos? new-count)
      ;; Добавляем или обновляем узел
      (let [new-node (Node. key new-count nil)]
        (if (nil? bucket)
          new-node
          (loop [current bucket
                 prev nil
                 found? false]
            (cond
              (nil? current)
              (if found?
                (if prev
                  (Node. (:key prev) (:count prev) new-node)
                  new-node)
                (Node. (:key prev) (:count prev) new-node))
              
              (eq-fn (:key current) key)
              (if prev
                (Node. (:key prev) (:count prev) 
                       (Node. key new-count (:next current)))
                (Node. key new-count (:next current)))
              
              :else
              (recur (:next current) current found?)))))
      ;; Удаляем узел (new-count <= 0)
      (if (nil? bucket)
        nil
        (loop [current bucket
               prev nil
               found? false]
          (cond
            (nil? current)
            (if prev prev nil)
            
            (eq-fn (:key current) key)
            (if prev
              (Node. (:key prev) (:count prev) (:next current))
              (:next current))
            
            :else
            (recur (:next current) current found?)))))))

(defn- bucket-size-change [old-bucket new-bucket key eq-fn]
  (let [old-count (loop [current old-bucket]
                    (if current
                      (if (eq-fn (:key current) key)
                        (:count current)
                        (recur (:next current)))
                      0))
        new-count (loop [current new-bucket]
                    (if current
                      (if (eq-fn (:key current) key)
                        (:count current)
                        (recur (:next current)))
                      0))]
    (cond
      (and (zero? old-count) (pos? new-count)) 1   ;; Добавлен новый элемент
      (and (pos? old-count) (zero? new-count)) -1  ;; Удален элемент
      :else 0)))                                   ;; Без изменений

;; === Функции высшего порядка (должны быть определены до их использования) ===

(defn reduce-left 
  "Левая свертка"
  [f init sc-bag]
  (loop [acc init
         i 0]
    (if (>= i (count (:buckets sc-bag)))
      acc
      (let [bucket (get (:buckets sc-bag) i)]
        (if (nil? bucket)
          (recur acc (inc i))
          (let [new-acc (loop [acc' acc
                               current bucket]
                          (if current
                            (let [acc'' (loop [acc''' acc'
                                               n (:count current)]
                                          (if (zero? n)
                                            acc'''
                                            (recur (f acc''' (:key current)) (dec n))))]
                              (recur acc'' (:next current)))
                            acc'))]
            (recur new-acc (inc i))))))))

(defn reduce-right 
  "Правая свертка"
  [f init sc-bag]
  (let [elements (reduce-left (fn [acc elem] (conj acc elem)) [] sc-bag)]
    (reduce f init (rseq elements))))

;; === Основной API ===

(defn empty-bag 
  "Создает пустой bag"
  ([] (empty-bag {}))
  ([{:keys [hash-fn eq-fn load-factor]
     :or {hash-fn default-hash
          eq-fn default-eq
          load-factor 0.75}}]
   (->SCBag (new-buckets 16) 0 hash-fn eq-fn load-factor)))

(defn bag 
  "Создает bag из коллекции элементов"
  ([coll] (bag coll {}))
  ([coll opts]
   (reduce (fn [acc x] 
             (let [hash-fn (:hash-fn acc)
                   eq-fn (:eq-fn acc)
                   buckets (:buckets acc)
                   bucket-idx (hash-bucket x (count buckets) hash-fn)
                   current-bucket (get buckets bucket-idx)
                   new-bucket (update-bucket current-bucket x inc eq-fn)]
               (-> acc
                   (assoc :buckets (assoc buckets bucket-idx new-bucket))
                   (update :size + (bucket-size-change current-bucket new-bucket x eq-fn)))))
           (empty-bag opts)
           coll)))

(defn bag-conj 
  "Добавляет элемент в bag"
  [sc-bag element]
  (let [hash-fn (:hash-fn sc-bag)
        eq-fn (:eq-fn sc-bag)
        buckets (:buckets sc-bag)
        bucket-idx (hash-bucket element (count buckets) hash-fn)
        current-bucket (get buckets bucket-idx)
        new-bucket (update-bucket current-bucket element inc eq-fn)]
    (-> sc-bag
        (assoc :buckets (assoc buckets bucket-idx new-bucket))
        (update :size + (bucket-size-change current-bucket new-bucket element eq-fn))
        ensure-capacity)))

(defn bag-disj 
  "Удаляет один экземпляр элемента из bag"
  [sc-bag element]
  (let [hash-fn (:hash-fn sc-bag)
        eq-fn (:eq-fn sc-bag)
        buckets (:buckets sc-bag)
        bucket-idx (hash-bucket element (count buckets) hash-fn)
        current-bucket (get buckets bucket-idx)
        new-bucket (update-bucket current-bucket element dec eq-fn)]
    (-> sc-bag
        (assoc :buckets (assoc buckets bucket-idx new-bucket))
        (update :size + (bucket-size-change current-bucket new-bucket element eq-fn)))))

(defn get-count 
  "Возвращает количество вхождений элемента"
  [sc-bag element]
  (if (instance? SCBag sc-bag)
    (let [hash-fn (:hash-fn sc-bag)
          eq-fn (:eq-fn sc-bag)
          buckets (:buckets sc-bag)
          bucket-idx (hash-bucket element (count buckets) hash-fn)
          bucket (get buckets bucket-idx)]
      (loop [current bucket]
        (cond
          (nil? current) 0
          (eq-fn (:key current) element) (:count current)
          :else (recur (:next current)))))
    ;; Если передан не SCBag, считаем это обычной коллекцией
    (get (frequencies sc-bag) element 0)))

(defn bag-contains? 
  "Проверяет наличие элемента в bag"
  [sc-bag element]
  (> (get-count sc-bag element) 0))

(defn count-elements 
  "Общее количество элементов (с учетом кратности)"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce + (for [bucket (:buckets sc-bag)
                    :when bucket]
                (loop [current bucket
                       total 0]
                  (if current
                    (recur (:next current) (+ total (:count current)))
                    total))))
    ;; Если передан не SCBag, считаем это обычной коллекцией
    (count sc-bag)))

(defn distinct-count 
  "Количество уникальных элементов"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (:size sc-bag)
    ;; Если передан не SCBag, считаем это обычной коллекцией
    (count (distinct sc-bag))))

;; === Функции высшего порядка ===

(defn bag-filter 
  "Фильтрация элементов bag"
  [pred sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left 
      (fn [acc elem]
        (if (pred elem)
          (bag-conj acc elem)
          acc))
      (empty-bag {:hash-fn (:hash-fn sc-bag) :eq-fn (:eq-fn sc-bag)})
      sc-bag)
    ;; Если передан не SCBag, фильтруем как обычную коллекцию
    (filter pred sc-bag)))

(defn bag-map 
  "Отображение функции на элементы bag"
  [f sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left 
      (fn [acc elem]
        (bag-conj acc (f elem)))
      (empty-bag {:hash-fn (:hash-fn sc-bag) :eq-fn (:eq-fn sc-bag)})
      sc-bag)
    ;; Если передан не SCBag, маппим как обычную коллекцию
    (map f sc-bag)))

;; === Интерфейс Seqable ===

(defn bag-seq 
  "Последовательность всех элементов (с учетом кратности)"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left (fn [acc elem] (conj acc elem)) [] sc-bag)
    ;; Если передан не SCBag, возвращаем как есть
    sc-bag))

(defn bag-distinct-seq 
  "Последовательность уникальных элементов"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left (fn [acc elem] (conj acc elem)) #{} sc-bag)
    ;; Если передан не SCBag, возвращаем уникальные элементы
    (distinct sc-bag)))

;; === Моноидные операции ===

(defn bag-union 
  "Объединение двух bags (моноидная операция)"
  [bag1 bag2]
  (let [all-elements (into #{} (concat (bag-seq bag1) (bag-seq bag2)))]
    (reduce 
      (fn [acc elem]
        (let [count1 (get-count bag1 elem)
              count2 (get-count bag2 elem)
              total (+ count1 count2)]
          (loop [result acc
                 n total]
            (if (zero? n)
              result
              (recur (bag-conj result elem) (dec n)))))))
      (empty-bag)
      all-elements))

;; === Сравнение ===

(defn bag-equals? 
  "Эффективное сравнение двух bags"
  [bag1 bag2]
  (and 
    (= (count-elements bag1) (count-elements bag2))
    (= (distinct-count bag1) (distinct-count bag2))
    (let [unique1 (bag-distinct-seq bag1)]
      (every? (fn [elem] 
                (= (get-count bag1 elem) (get-count bag2 elem))) 
              unique1))))

;; === Утилиты ===

(defn bag-frequencies 
  "Возвращает map с частотами элементов"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left (fn [acc elem] 
                   (update acc elem (fnil inc 0))) 
                 {} sc-bag)
    ;; Если передан не SCBag, используем стандартную frequencies
    (frequencies sc-bag)))

(defn bag->vector [sc-bag]
  (vec (bag-seq sc-bag)))

(defn bag->string [sc-bag]
  (str "#Bag" (bag-frequencies sc-bag)))

;; === Утилиты для вывода ===

(defmethod print-method SCBag [bag writer]
  (.write writer (bag->string bag)))