(ns sc-bag.core
  "Реализация Bag (multiset) на основе Separate Chaining Hashmap"
  (:import [clojure.lang Seqable IPersistentCollection Counted ILookup IFn Associative]))

;; Внутреннее представление 

(defrecord Node [key count next])

;; Forward declarations для функций, используемых в методах интерфейсов
(declare bag-seq count-elements bag-conj empty-bag bag-equals? get-count bag-contains?)

;; Определяем SCBag через deftype с полной реализацией стандартных интерфейсов коллекций Clojure
(deftype SCBag [buckets size hash-fn eq-fn load-factor]
  ;; Seqable - для работы с seq
  clojure.lang.Seqable
  (seq [this]
    (let [s (bag-seq this)]
      (if (empty? s) nil s)))

  ;; Counted - для работы с count
  clojure.lang.Counted
  (count [this]
    (count-elements this))

  ;; IPersistentCollection - для работы с cons, empty, equiv
  clojure.lang.IPersistentCollection
  (cons [this o]
    (bag-conj this o))
  (empty [this]
    (empty-bag {:hash-fn (.hash-fn this) :eq-fn (.eq-fn this) :load-factor (.load-factor this)}))
  (equiv [this o]
    (if (instance? SCBag o)
      (bag-equals? this o)
      false))

  ;; ILookup - для работы с get
  clojure.lang.ILookup
  (valAt [this key]
    ;; Если key - это ключевое слово (поле записи), используем прямое обращение
    (if (keyword? key)
      (case key
        :buckets (.buckets this)
        :size (.size this)
        :hash-fn (.hash-fn this)
        :eq-fn (.eq-fn this)
        :load-factor (.load-factor this)
        nil)
      ;; Иначе работаем с элементами bag
      (get-count this key)))
  (valAt [this key not-found]
    (if (keyword? key)
      (case key
        :buckets (.buckets this)
        :size (.size this)
        :hash-fn (.hash-fn this)
        :eq-fn (.eq-fn this)
        :load-factor (.load-factor this)
        not-found)
      (let [cnt (get-count this key)]
        (if (zero? cnt)
          not-found
          cnt))))

  ;; IFn - для вызова bag как функции
  clojure.lang.IFn
  (invoke [this key]
    (get-count this key))
  (invoke [this key not-found]
    (let [cnt (get-count this key)]
      (if (zero? cnt)
        not-found
        cnt)))

  ;; Associative - для работы с contains?, find
  clojure.lang.Associative
  (containsKey [this key]
    ;; Если key - это ключевое слово (поле записи), проверяем напрямую
    (if (keyword? key)
      (contains? #{:buckets :size :hash-fn :eq-fn :load-factor} key)
      ;; Иначе работаем с элементами bag
      (bag-contains? this key)))
  (entryAt [this key]
    (if (keyword? key)
      (case key
        :buckets (clojure.lang.MapEntry. :buckets (.buckets this))
        :size (clojure.lang.MapEntry. :size (.size this))
        :hash-fn (clojure.lang.MapEntry. :hash-fn (.hash-fn this))
        :eq-fn (clojure.lang.MapEntry. :eq-fn (.eq-fn this))
        :load-factor (clojure.lang.MapEntry. :load-factor (.load-factor this))
        nil)
      (let [cnt (get-count this key)]
        (if (pos? cnt)
          (clojure.lang.MapEntry. key cnt)
          nil)))))

;; Вспомогательные функции

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
  (let [current-capacity (count (.buckets sc-bag))
        current-size (.size sc-bag)
        load-factor (.load-factor sc-bag)]
    (if (and (> current-capacity 0)
             (>= (/ current-size current-capacity) load-factor))
      (let [new-capacity (* 2 current-capacity)
            hash-fn (.hash-fn sc-bag)
            eq-fn (.eq-fn sc-bag)]

        ;; Перехешируем все элементы (неизменяемо)
        (loop [buckets (new-buckets new-capacity)
               i 0]
          (if (>= i (count (.buckets sc-bag)))
            (SCBag. buckets current-size hash-fn eq-fn load-factor)
            (let [bucket (get (.buckets sc-bag) i)]
              (if (nil? bucket)
                (recur buckets (inc i))
                (let [new-buckets' (loop [current bucket
                                          buckets' buckets]
                                     (if (nil? current)
                                       buckets'
                                       (let [bucket-idx (hash-bucket (:key current) new-capacity hash-fn)
                                             existing (get buckets' bucket-idx)
                                             new-node (Node. (:key current) (:count current) existing)]
                                         (recur (:next current)
                                                (assoc buckets' bucket-idx new-node)))))]
                  (recur new-buckets' (inc i))))))))
      sc-bag)))

(defn- update-bucket [bucket key count-update eq-fn]
  (let [old-count (loop [current bucket]
                    (if current
                      (if (eq-fn (:key current) key)
                        (:count current)
                        (recur (:next current)))
                      0))
        new-count (count-update old-count)]
    (if (pos? new-count)
      ;; Добавляем или обновляем узел
      (if (nil? bucket)
        (Node. key new-count nil)
        (letfn [(build-list [current found?]
                  (cond
                    (nil? current)
                    (if found?
                      nil
                      (Node. key new-count nil))

                    (eq-fn (:key current) key)
                    (Node. key new-count (build-list (:next current) true))

                    :else
                    (Node. (:key current) (:count current)
                           (build-list (:next current) found?))))]
          (build-list bucket false)))
      ;; Удаляем узел (new-count <= 0)
      (if (nil? bucket)
        nil
        (letfn [(build-list [current found?]
                  (cond
                    (nil? current)
                    nil

                    (eq-fn (:key current) key)
                    (build-list (:next current) true)

                    :else
                    (Node. (:key current) (:count current)
                           (build-list (:next current) found?))))]
          (let [result (build-list bucket false)
                was-found? (not= old-count 0)]
            (if (and (nil? result) (not was-found?))
              bucket
              result)))))))

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

;; Функции высшего порядка 

(defn reduce-left
  "Левая свертка"
  [f init sc-bag]
  (loop [acc init
         i 0]
    (if (>= i (count (.buckets sc-bag)))
      acc
      (let [bucket (get (.buckets sc-bag) i)]
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

;; API SCBag - Специфичные функции для работы с bag

(defn empty-bag
  "Создает пустой bag"
  ([] (empty-bag {}))
  ([{:keys [hash-fn eq-fn load-factor]
     :or {hash-fn default-hash
          eq-fn default-eq
          load-factor 0.75}}]
   (SCBag. (new-buckets 16) 0 hash-fn eq-fn load-factor)))

(defn bag
  "Создает bag из коллекции элементов"
  ([coll] (bag coll {}))
  ([coll opts]
   (reduce (fn [acc x]
             (let [hash-fn (.hash-fn acc)
                   eq-fn (.eq-fn acc)
                   buckets (.buckets acc)
                   bucket-idx (hash-bucket x (count buckets) hash-fn)
                   current-bucket (get buckets bucket-idx)
                   new-bucket (update-bucket current-bucket x inc eq-fn)
                   new-buckets (assoc buckets bucket-idx new-bucket)
                   size-change (bucket-size-change current-bucket new-bucket x eq-fn)
                   new-size (+ (.size acc) size-change)]
               (SCBag. new-buckets new-size hash-fn eq-fn (.load-factor acc))))
           (empty-bag opts)
           coll)))

(defn bag-conj
  "Добавляет элемент в bag"
  [sc-bag element]
  (let [hash-fn (.hash-fn sc-bag)
        eq-fn (.eq-fn sc-bag)
        buckets (.buckets sc-bag)
        bucket-idx (hash-bucket element (count buckets) hash-fn)
        current-bucket (get buckets bucket-idx)
        new-bucket (update-bucket current-bucket element inc eq-fn)
        new-buckets (assoc buckets bucket-idx new-bucket)
        size-change (bucket-size-change current-bucket new-bucket element eq-fn)
        new-size (+ (.size sc-bag) size-change)
        new-bag (SCBag. new-buckets new-size hash-fn eq-fn (.load-factor sc-bag))]
    (ensure-capacity new-bag)))

(defn bag-disj
  "Удаляет один экземпляр элемента из bag"
  [sc-bag element]
  (let [hash-fn (.hash-fn sc-bag)
        eq-fn (.eq-fn sc-bag)
        buckets (.buckets sc-bag)
        bucket-idx (hash-bucket element (count buckets) hash-fn)
        current-bucket (get buckets bucket-idx)
        new-bucket (update-bucket current-bucket element dec eq-fn)
        new-buckets (assoc buckets bucket-idx new-bucket)
        size-change (bucket-size-change current-bucket new-bucket element eq-fn)
        new-size (+ (.size sc-bag) size-change)]
    (SCBag. new-buckets new-size hash-fn eq-fn (.load-factor sc-bag))))

(defn bag-filter
  "Фильтрация элементов bag"
  [pred sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left
     (fn [acc elem]
       (if (pred elem)
         (bag-conj acc elem)
         acc))
     (empty-bag {:hash-fn (.hash-fn sc-bag) :eq-fn (.eq-fn sc-bag)})
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
     (empty-bag {:hash-fn (.hash-fn sc-bag) :eq-fn (.eq-fn sc-bag)})
     sc-bag)
    ;; Если передан не SCBag, маппим как обычную коллекцию
    (map f sc-bag)))

;; Интеграция со стандартными коллекциями Clojure
;; Функции, которые работают как с SCBag, так и с обычными коллекциями

(defn get-count
  "Возвращает количество вхождений элемента.
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag element]
  (if (instance? SCBag sc-bag)
    (let [hash-fn (.hash-fn sc-bag)
          eq-fn (.eq-fn sc-bag)
          buckets (.buckets sc-bag)
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
  "Проверяет наличие элемента в bag.
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag element]
  (> (get-count sc-bag element) 0))

(defn count-elements
  "Общее количество элементов (с учетом кратности).
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce + (for [bucket (.buckets sc-bag)
                    :when bucket]
                (loop [current bucket
                       total 0]
                  (if current
                    (recur (:next current) (+ total (:count current)))
                    total))))
    ;; Если передан не SCBag, считаем это обычной коллекцией
    (count sc-bag)))

(defn distinct-count
  "Количество уникальных элементов.
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (.size sc-bag)
    ;; Если передан не SCBag, считаем это обычной коллекцией
    (count (distinct sc-bag))))

(defn bag-seq
  "Последовательность всех элементов (с учетом кратности).
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag]
  (if (instance? SCBag sc-bag)
    ;; Явно создаем ленивую последовательность
    (letfn [(walk-buckets [buckets idx]
              (lazy-seq
               (when (< idx (count buckets))
                 (let [bucket (get buckets idx)]
                   (if (nil? bucket)
                     (walk-buckets buckets (inc idx))
                     (letfn [(walk-nodes [node]
                               (lazy-seq
                                (when node
                                  (let [key (:key node)
                                        cnt (:count node)]
                                    (if (pos? cnt)
                                     ;; Повторяем элемент cnt раз
                                      (concat (repeat cnt key)
                                              (walk-nodes (:next node)))
                                      (walk-nodes (:next node)))))))]
                       (concat (walk-nodes bucket)
                               (walk-buckets buckets (inc idx)))))))))]
      (walk-buckets (.buckets sc-bag) 0))
    ;; Если передан не SCBag, возвращаем как есть
    (seq sc-bag)))

(defn bag-distinct-seq
  "Последовательность уникальных элементов.
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (letfn [(walk-buckets [buckets idx]
              (lazy-seq
               (when (< idx (count buckets))
                 (let [bucket (get buckets idx)]
                   (if (nil? bucket)
                     (walk-buckets buckets (inc idx))
                     (letfn [(walk-nodes [node]
                               (lazy-seq
                                (when node
                                  (cons (:key node) (walk-nodes (:next node))))))]
                       (concat (walk-nodes bucket)
                               (walk-buckets buckets (inc idx)))))))))]
      (walk-buckets (.buckets sc-bag) 0))
    ;; Если передан не SCBag, возвращаем уникальные элементы
    (distinct sc-bag)))

(defn bag-frequencies
  "Возвращает map с частотами элементов.
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (reduce-left (fn [acc elem]
                   (update acc elem (fnil inc 0)))
                 {} sc-bag)
    ;; Если передан не SCBag, используем стандартную frequencies
    (frequencies sc-bag)))

(defn bag-union
  "Объединение двух bags (моноидная операция).
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [bag1 bag2]
  (let [all-elements (into #{} (concat (bag-seq bag1) (bag-seq bag2)))
        ;; Используем параметры из bag1, если он не пустой, иначе из bag2
        opts (if (instance? SCBag bag1)
               {:hash-fn (.hash-fn bag1) :eq-fn (.eq-fn bag1)}
               (if (instance? SCBag bag2)
                 {:hash-fn (.hash-fn bag2) :eq-fn (.eq-fn bag2)}
                 {}))]
    (if (empty? all-elements)
      (empty-bag opts)
      (reduce
       (fn [acc elem]
         (let [count1 (get-count bag1 elem)
               count2 (get-count bag2 elem)
               total (+ count1 count2)]
           (loop [result acc
                  n total]
             (if (zero? n)
               result
               (recur (bag-conj result elem) (dec n))))))
       (empty-bag opts)
       all-elements))))

(defn bag-equals?
  "Эффективное сравнение двух bags.
   Работает как с SCBag, так и с обычными коллекциями Clojure."
  [bag1 bag2]
  (and
   (= (count-elements bag1) (count-elements bag2))
   (= (distinct-count bag1) (distinct-count bag2))
   (let [unique1 (bag-distinct-seq bag1)]
     (every? (fn [elem]
               (= (get-count bag1 elem) (get-count bag2 elem)))
             unique1))))

;; Утилиты

(defn bag->vector [sc-bag]
  (vec (bag-seq sc-bag)))

(defn bag->string [sc-bag]
  (str "#Bag" (bag-frequencies sc-bag)))

(defmethod print-method SCBag [bag writer]
  (.write writer (bag->string bag)))
