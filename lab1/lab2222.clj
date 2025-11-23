```clojure
(ns sc-bag.core
  "Реализация Bag (multiset) на основе Separate Chaining Hashmap")

;; == ВНУТРЕННЕЕ ПРЕДСТАВЛЕНИЕ ДАННЫХ ==

;; Запись Node представляет узел в связанном списке внутри бакета хеш-таблицы
;; - key: значение элемента
;; - count: количество повторений этого элемента в bag
;; - next: ссылка на следующий узел в цепочке коллизий
(defrecord Node [key count next])

;; Запись SCBag представляет всю структуру bag
;; - buckets: вектор бакетов (каждый бакет - связанный список Node или nil)
;; - size: количество уникальных элементов (размер множества ключей)
;; - hash-fn: функция для вычисления хеша ключей
;; - eq-fn: функция для сравнения ключей на равенство
;; - load-factor: коэффициент загрузки для определения необходимости рехеширования
(defrecord SCBag [buckets size hash-fn eq-fn load-factor])

;; == ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ==

;; Вычисляет индекс бакета для ключа
;; - key: элемент для хеширования
;; - num-buckets: общее количество бакетов
;; - hash-fn: функция хеширования
(defn- hash-bucket [key num-buckets hash-fn]
  (-> (hash-fn key)      ; Вычисляем хеш ключа
      (mod num-buckets))) ; Берем остаток от деления на количество бакетов

;; Стандартная функция хеширования по умолчанию
;; - x: любой объект Clojure
(defn- default-hash [x]
  (hash x)) ; Используем встроенную функцию hash

;; Стандартная функция сравнения на равенство по умолчанию
;; - a, b: объекты для сравнения
(defn- default-eq [a b]
  (= a b)) ; Используем встроенную функцию =

;; Создает новый вектор бакетов заданной емкости
;; - capacity: количество бакетов
(defn- new-buckets [capacity]
  (vec (repeat capacity nil))) ; Создаем вектор из capacity элементов nil

;; Проверяет необходимость увеличения размера таблицы и выполняет рехеширование
;; - sc-bag: текущий bag
(defn- ensure-capacity [sc-bag]
  (let [current-capacity (count (:buckets sc-bag)) ; Текущее количество бакетов
        current-size (:size sc-bag)                ; Количество уникальных элементов
        load-factor (:load-factor sc-bag)]         ; Коэффициент загрузки
    ;; Проверяем, превышена ли максимальная нагрузка
    (if (and (> current-capacity 0)
             (>= (/ current-size current-capacity) load-factor))
      ;; Если да - выполняем рехеширование
      (let [new-capacity (* 2 current-capacity) ; Удваиваем емкость
            hash-fn (:hash-fn sc-bag)           ; Получаем функцию хеширования
            eq-fn (:eq-fn sc-bag)]              ; Получаем функцию сравнения
        
        ;; Перехешируем все элементы (неизменяемо)
        (loop [buckets (new-buckets new-capacity) ; Новая пустая таблица
               i 0]                               ; Индекс текущего бакета в старой таблице
          (if (>= i (count (:buckets sc-bag)))    ; Если прошли все бакеты старой таблицы
            ;; Возвращаем новый bag с перераспределенными элементами
            (->SCBag buckets current-size hash-fn eq-fn load-factor)
            ;; Обрабатываем текущий бакет
            (let [bucket (get (:buckets sc-bag) i)] ; Получаем бакет из старой таблицы
              (if (nil? bucket) ; Если бакет пустой
                (recur buckets (inc i)) ; Переходим к следующему бакету
                ;; Если в бакете есть элементы, перераспределяем их
                (let [new-buckets' (loop [current bucket     ; Текущий узел в цепочке
                                          buckets' buckets] ; Аккумулятор новой таблицы
                                     (if (nil? current) ; Если цепочка закончилась
                                       buckets'         ; Возвращаем новую таблицу
                                       ;; Перемещаем текущий узел в новый бакет
                                       (let [bucket-idx (hash-bucket (:key current) new-capacity hash-fn) ; Новый индекс
                                             existing (get buckets' bucket-idx) ; Существующая цепочка в новом бакете
                                             new-node (Node. (:key current) (:count current) existing)] ; Создаем новый узел
                                         (recur (:next current)                    ; Переходим к следующему узлу
                                                (assoc buckets' bucket-idx new-node)))))] ; Обновляем новый бакет
                  (recur new-buckets' (inc i)))))))) ; Переходим к следующему бакету
      ;; Если рехеширование не требуется, возвращаем исходный bag
      sc-bag)))

;; Обновляет цепочку в бакете для заданного ключа
;; - bucket: текущая цепочка узлов (или nil)
;; - key: ключ для обновления
;; - count-update: функция обновления счетчика (inc/dec)
;; - eq-fn: функция сравнения ключей
(defn- update-bucket [bucket key count-update eq-fn]
  ;; Сначала находим текущее количество для ключа
  (let [old-count (loop [current bucket] ; Проходим по цепочке
                    (if current
                      (if (eq-fn (:key current) key) ; Если нашли ключ
                        (:count current)             ; Возвращаем его количество
                        (recur (:next current)))     ; Ищем дальше
                      0)) ; Если не нашли - количество 0
        new-count (count-update old-count)] ; Вычисляем новое количество
    (if (pos? new-count) ; Если новое количество положительное
      ;; Добавляем или обновляем узел
      (if (nil? bucket) ; Если цепочка пустая
        (Node. key new-count nil) ; Создаем новый узел
        ;; Рекурсивно строим новую цепочку
        (letfn [(build-list [current found?] ; current - текущий узел, found? - нашли ли ключ
                  (cond
                    ;; Дошли до конца цепочки
                    (nil? current)
                    (if found?
                      nil ; Если уже нашли и обновили ключ - конец цепочки
                      (Node. key new-count nil)) ; Если не нашли - добавляем в конец

                    ;; Нашли нужный ключ
                    (eq-fn (:key current) key)
                    (Node. key new-count (build-list (:next current) true)) ; Обновляем счетчик

                    ;; Другой ключ - продолжаем построение
                    :else
                    (Node. (:key current) (:count current) ; Сохраняем текущий узел
                           (build-list (:next current) found?))))] ; Продолжаем с остатком
          (build-list bucket false))) ; Начинаем построение с головы цепочки
      ;; Удаляем узел (new-count <= 0)
      (if (nil? bucket) ; Если цепочка пустая
        nil ; Нечего удалять
        ;; Рекурсивно строим новую цепочку без удаленного ключа
        (letfn [(build-list [current found?]
                  (cond
                    (nil? current) ; Конец цепочки
                    nil

                    (eq-fn (:key current) key) ; Нашли ключ для удаления
                    (build-list (:next current) true) ; Пропускаем его

                    :else ; Сохраняем текущий узел
                    (Node. (:key current) (:count current)
                           (build-list (:next current) found?))))]
          (let [result (build-list bucket false) ; Строим новую цепочку
                was-found? (not= old-count 0)] ; Был ли ключ в исходной цепочке
            ;; Если результат nil и ключа не было - возвращаем исходную цепочку
            (if (and (nil? result) (not was-found?))
              bucket
              result)))))))

;; Вычисляет изменение количества уникальных элементов при обновлении бакета
;; - old-bucket: исходная цепочка
;; - new-bucket: новая цепочка после обновления
;; - key: обрабатываемый ключ
;; - eq-fn: функция сравнения
(defn- bucket-size-change [old-bucket new-bucket key eq-fn]
  ;; Находим количество в старой цепочке
  (let [old-count (loop [current old-bucket]
                    (if current
                      (if (eq-fn (:key current) key)
                        (:count current)
                        (recur (:next current)))
                      0))
        ;; Находим количество в новой цепочке
        new-count (loop [current new-bucket]
                    (if current
                      (if (eq-fn (:key current) key)
                        (:count current)
                        (recur (:next current)))
                      0))]
    (cond
      ;; Элемент добавлен (не было -> есть)
      (and (zero? old-count) (pos? new-count)) 1
      ;; Элемент удален (был -> нет)
      (and (pos? old-count) (zero? new-count)) -1
      ;; Без изменений в уникальных элементах
      :else 0)))

;; == ФУНКЦИИ ВЫСШЕГО ПОРЯДКА ==

;; Левая свертка (foldl) по всем элементам bag с учетом кратности
;; - f: функция свертки (acc, element) -> new-acc
;; - init: начальное значение аккумулятора
;; - sc-bag: bag для обхода
(defn reduce-left
  "Левая свертка"
  [f init sc-bag]
  (loop [acc init         ; Текущий аккумулятор
         i 0]             ; Индекс текущего бакета
    (if (>= i (count (:buckets sc-bag))) ; Если прошли все бакеты
      acc ; Возвращаем результат
      (let [bucket (get (:buckets sc-bag) i)] ; Получаем текущий бакет
        (if (nil? bucket) ; Если бакет пустой
          (recur acc (inc i)) ; Переходим к следующему бакету
          ;; Обрабатываем непустой бакет
          (let [new-acc (loop [acc' acc           ; Аккумулятор для бакета
                               current bucket]    ; Текущий узел в цепочке
                          (if current ; Если узел существует
                            ;; Обрабатываем все повторения элемента
                            (let [acc'' (loop [acc''' acc'    ; Аккумулятор для повторений
                                               n (:count current)] ; Количество оставшихся повторений
                                          (if (zero? n) ; Если обработали все повторения
                                            acc''' ; Возвращаем аккумулятор
                                            ;; Применяем функцию к очередному повторению
                                            (recur (f acc''' (:key current)) (dec n))))]
                              ;; Переходим к следующему узлу в цепочке
                              (recur acc'' (:next current)))
                            ;; Если цепочка закончилась
                            acc'))]
            ;; Переходим к следующему бакету
            (recur new-acc (inc i))))))))

;; Правая свертка (foldr) по всем элементам bag
;; - f: функция свертки (element, acc) -> new-acc  
;; - init: начальное значение аккумулятора
;; - sc-bag: bag для обхода
(defn reduce-right
  "Правая свертка"
  [f init sc-bag]
  ;; Сначала собираем все элементы в вектор, затем применяем правую свертку
  (let [elements (reduce-left (fn [acc elem] (conj acc elem)) [] sc-bag)]
    (reduce f init (rseq elements)))) ; rseq дает обратную последовательность

;; == ОСНОВНОЙ API ==

;; Создает пустой bag
;; Можно передать опции: :hash-fn, :eq-fn, :load-factor
(defn empty-bag
  "Создает пустой bag"
  ([] (empty-bag {})) ; Без аргументов - используем опции по умолчанию
  ([{:keys [hash-fn eq-fn load-factor] ; Деструктурируем опции
     :or {hash-fn default-hash         ; Значения по умолчанию
          eq-fn default-eq
          load-factor 0.75}}]
   ;; Создаем bag с начальной емкостью 16 бакетов
   (->SCBag (new-buckets 16) 0 hash-fn eq-fn load-factor)))

;; Создает bag из коллекции элементов
(defn bag
  "Создает bag из коллекции элементов"
  ([coll] (bag coll {})) ; Без опций
  ([coll opts] ; С коллекцией и опциями
   ;; Используем reduce для построения bag из коллекции
   (reduce (fn [acc x] ; acc - накапливаемый bag, x - текущий элемент
             (let [hash-fn (:hash-fn acc)        ; Функция хеширования
                   eq-fn (:eq-fn acc)            ; Функция сравнения
                   buckets (:buckets acc)        ; Текущие бакеты
                   bucket-idx (hash-bucket x (count buckets) hash-fn) ; Индекс бакета
                   current-bucket (get buckets bucket-idx) ; Текущая цепочка
                   new-bucket (update-bucket current-bucket x inc eq-fn)] ; Новая цепочка
               ;; Обновляем bag
               (-> acc
                   (assoc :buckets (assoc buckets bucket-idx new-bucket)) ; Обновляем бакеты
                   (update :size + (bucket-size-change current-bucket new-bucket x eq-fn))))) ; Обновляем размер
           (empty-bag opts) ; Начальное значение - пустой bag
           coll))) ; Коллекция для обработки

;; Добавляет элемент в bag
(defn bag-conj
  "Добавляет элемент в bag"
  [sc-bag element]
  (let [hash-fn (:hash-fn sc-bag)
        eq-fn (:eq-fn sc-bag)
        buckets (:buckets sc-bag)
        bucket-idx (hash-bucket element (count buckets) hash-fn)
        current-bucket (get buckets bucket-idx)
        new-bucket (update-bucket current-bucket element inc eq-fn)] ; Увеличиваем счетчик
    (-> sc-bag
        (assoc :buckets (assoc buckets bucket-idx new-bucket)) ; Обновляем бакеты
        (update :size + (bucket-size-change current-bucket new-bucket element eq-fn)) ; Обновляем размер
        ensure-capacity))) ; Проверяем необходимость рехеширования

;; Удаляет один экземпляр элемента из bag
(defn bag-disj
  "Удаляет один экземпляр элемента из bag"
  [sc-bag element]
  (let [hash-fn (:hash-fn sc-bag)
        eq-fn (:eq-fn sc-bag)
        buckets (:buckets sc-bag)
        bucket-idx (hash-bucket element (count buckets) hash-fn)
        current-bucket (get buckets bucket-idx)
        new-bucket (update-bucket current-bucket element dec eq-fn)] ; Уменьшаем счетчик
    (-> sc-bag
        (assoc :buckets (assoc buckets bucket-idx new-bucket))
        (update :size + (bucket-size-change current-bucket new-bucket element eq-fn)))))

;; Возвращает количество вхождений элемента
(defn get-count
  "Возвращает количество вхождений элемента"
  [sc-bag element]
  (if (instance? SCBag sc-bag) ; Если передан SCBag
    ;; Используем внутреннее представление для эффективного поиска
    (let [hash-fn (:hash-fn sc-bag)
          eq-fn (:eq-fn sc-bag)
          buckets (:buckets sc-bag)
          bucket-idx (hash-bucket element (count buckets) hash-fn)
          bucket (get buckets bucket-idx)]
      ;; Ищем элемент в цепочке бакета
      (loop [current bucket]
        (cond
          (nil? current) 0 ; Элемент не найден
          (eq-fn (:key current) element) (:count current) ; Нашли - возвращаем счетчик
          :else (recur (:next current))))) ; Продолжаем поиск
    ;; Если передан не SCBag, используем стандартную frequencies
    (get (frequencies sc-bag) element 0)))

;; Проверяет наличие элемента в bag (хотя бы одного вхождения)
(defn bag-contains?
  "Проверяет наличие элемента в bag"
  [sc-bag element]
  (> (get-count sc-bag element) 0)) ; Элемент есть если count > 0

;; Общее количество элементов с учетом кратности
(defn count-elements
  "Общее количество элементов (с учетом кратности)"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    ;; Для SCBag: суммируем все счетчики во всех бакетах
    (reduce + (for [bucket (:buckets sc-bag) ; Для каждого бакета
                    :when bucket]            ; Который не nil
                (loop [current bucket
                       total 0]
                  (if current
                    (recur (:next current) (+ total (:count current))) ; Суммируем счетчики
                    total))))
    ;; Для обычной коллекции: используем count
    (count sc-bag)))

;; Количество уникальных элементов
(defn distinct-count
  "Количество уникальных элементов"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    (:size sc-bag) ; Для SCBag это поле size
    ;; Для обычной коллекции: используем count и distinct
    (count (distinct sc-bag))))

;; == ФУНКЦИИ ВЫСШЕГО ПОРЯДКА ==

;; Фильтрация элементов bag по предикату
(defn bag-filter
  "Фильтрация элементов bag"
  [pred sc-bag] ; pred - функция-предикат (element -> boolean)
  (if (instance? SCBag sc-bag)
    ;; Для SCBag: используем reduce-left для построения нового bag
    (reduce-left
     (fn [acc elem]
       (if (pred elem)
         (bag-conj acc elem) ; Если элемент проходит фильтр - добавляем
         acc))               ; Иначе - пропускаем
     (empty-bag {:hash-fn (:hash-fn sc-bag) :eq-fn (:eq-fn sc-bag)}) ; Новый bag с теми же настройками
     sc-bag)
    ;; Для обычной коллекции: используем стандартный filter
    (filter pred sc-bag)))

;; Применение функции к каждому элементу bag
(defn bag-map
  "Отображение функции на элементы bag"
  [f sc-bag] ; f - функция отображения (element -> new-element)
  (if (instance? SCBag sc-bag)
    ;; Для SCBag: строим новый bag с преобразованными элементами
    (reduce-left
     (fn [acc elem]
       (bag-conj acc (f elem))) ; Добавляем преобразованный элемент
     (empty-bag {:hash-fn (:hash-fn sc-bag) :eq-fn (:eq-fn sc-bag)})
     sc-bag)
    ;; Для обычной коллекции: используем стандартный map
    (map f sc-bag)))

;; == ИНТЕРФЕЙС SEQABLE ==

;; Преобразует bag в последовательность всех элементов (с учетом кратности)
(defn bag-seq
  "Последовательность всех элементов (с учетом кратности)"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    ;; Для SCBag: используем reduce-left для построения вектора
    (reduce-left (fn [acc elem] (conj acc elem)) [] sc-bag)
    ;; Для обычной коллекции: возвращаем как есть
    sc-bag))

;; Последовательность уникальных элементов
(defn bag-distinct-seq
  "Последовательность уникальных элементов"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    ;; Для SCBag: используем reduce-left для построения множества
    (reduce-left (fn [acc elem] (conj acc elem)) #{} sc-bag)
    ;; Для обычной коллекции: используем distinct
    (distinct sc-bag)))

;; == МОНОИДНЫЕ ОПЕРАЦИИ ==

;; Объединение двух bags (суммирование количеств для каждого элемента)
(defn bag-union
  "Объединение двух bags (моноидная операция)"
  [bag1 bag2]
  ;; Собираем множество всех уникальных элементов из обоих bags
  (let [all-elements (into #{} (concat (bag-seq bag1) (bag-seq bag2)))
        ;; Выбираем настройки хеширования из первого непустого bag
        opts (if (instance? SCBag bag1)
               {:hash-fn (:hash-fn bag1) :eq-fn (:eq-fn bag1)}
               (if (instance? SCBag bag2)
                 {:hash-fn (:hash-fn bag2) :eq-fn (:eq-fn bag2)}
                 {}))]
    (if (empty? all-elements)
      (empty-bag opts) ; Если нет элементов - пустой bag
      ;; Строим новый bag с суммарными количествами
      (reduce
       (fn [acc elem]
         (let [count1 (get-count bag1 elem) ; Количество в первом bag
               count2 (get-count bag2 elem) ; Количество во втором bag  
               total (+ count1 count2)]     ; Суммарное количество
           ;; Добавляем элемент total раз
           (loop [result acc
                  n total]
             (if (zero? n)
               result
               (recur (bag-conj result elem) (dec n))))))
       (empty-bag opts)
       all-elements))))

;; == СРАВНЕНИЕ ==

;; Эффективное сравнение двух bags на равенство
(defn bag-equals?
  "Эффективное сравнение двух bags"
  [bag1 bag2]
  (and
   ;; Совпадает общее количество элементов
   (= (count-elements bag1) (count-elements bag2))
   ;; Совпадает количество уникальных элементов
   (= (distinct-count bag1) (distinct-count bag2))
   ;; Для каждого уникального элемента количества совпадают
   (let [unique1 (bag-distinct-seq bag1)] ; Уникальные элементы первого bag
     (every? (fn [elem]
               (= (get-count bag1 elem) (get-count bag2 elem)))
             unique1))))

;; == УТИЛИТЫ ==

;; Возвращает map с частотами элементов
(defn bag-frequencies
  "Возвращает map с частотами элементов"
  [sc-bag]
  (if (instance? SCBag sc-bag)
    ;; Для SCBag: используем reduce-left для построения map частот
    (reduce-left (fn [acc elem]
                   (update acc elem (fnil inc 0))) ; Увеличиваем счетчик для элемента
                 {} sc-bag)
    ;; Для обычной коллекции: используем стандартную frequencies
    (frequencies sc-bag)))

;; Преобразует bag в вектор
(defn bag->vector [sc-bag]
  (vec (bag-seq sc-bag))) ; Просто преобразуем последовательность в вектор

;; Строковое представление bag
(defn bag->string [sc-bag]
  (str "#Bag" (bag-frequencies sc-bag))) ; Формат: #Bag{элемент количество, ...}

;; == ВЫВОД ДЛЯ REPL ==

;; Метод для красивого вывода bag в REPL
(defmethod print-method SCBag [bag writer]
  (.write writer (bag->string bag)))
```