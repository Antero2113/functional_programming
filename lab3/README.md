# Лабораторная работа №3: Интерполяция данных в потоковом режиме
Карандашева Анастасия, 368273, группа P3332

### Цель работы
Получить навыки работы с вводом/выводом, потоковой обработкой данных, командной строкой. Реализовать программу для интерполяции данных, работающую в потоковом режиме.

### Основные требования

1. **Реализованные алгоритмы интерполяции:**
   - Линейная интерполяция (отрезками)
   - Интерполяция методом Ньютона (с настраиваемым количеством точек)

2. **Настройки через аргументы командной строки:**
   - Выбор алгоритмов интерполяции (`--linear`, `--newton`, возможно оба одновременно)
   - Частота дискретизации (`--step <значение>`)
   - Количество точек для метода Ньютона (`-n <значение>`)

3. **Формат входных данных:**
   - Текстовый формат на подобии CSV (поддерживаются форматы: `x;y`, `x\ty`, `x y`)
   - Данные подаются на стандартный ввод
   - Входные данные отсортированы по возрастанию x

4. **Формат выходных данных:**
   - Вывод на стандартный вывод
   - Формат: `алгоритм: x y` (например, `linear: 0.0 0.0`)
   - Округление до 1 десятичного знака

5. **Потоковый режим работы:**
   - Программа ожидает данные на стандартном вводе
   - По мере получения достаточного количества данных выводит рассчитанные точки
   - Используется модель "скользящего окна": старые данные удаляются, новые добавляются

### Описание алгоритмов

#### Линейная интерполяция

**Алгоритм:** Для двух точек `(x₁, y₁)` и `(x₂, y₂)` значение `y` для точки `x` вычисляется по формуле:

```
y = y₁ + (y₂ - y₁) * (x - x₁) / (x₂ - x₁)
```

**Окно данных:** Используются последние 2 точки из буфера.

**Особенности:**
- Расчет начинается после получения 2 точек
- Для каждого нового сегмента генерируются промежуточные точки с заданным шагом от первой до второй точки окна
- После обработки окна удаляются все точки до последней точки окна включительно для оптимизации памяти
- При EOF генерируются все оставшиеся точки от `last-x + step` до последней точки окна включительно

#### Интерполяция методом Ньютона

**Алгоритм:** Используется полином Ньютона с разделенными разностями. Для набора точек `(x₀, y₀), (x₁, y₁), ..., (xₙ, yₙ)` значение `y` для точки `x` вычисляется по формуле:

```
P(x) = f[x₀] + f[x₀,x₁](x-x₀) + f[x₀,x₁,x₂](x-x₀)(x-x₁) + ... + f[x₀,...,xₙ](x-x₀)...(x-xₙ₋₁)
```

где `f[x₀,...,xᵢ]` — разделенные разности, вычисляемые рекурсивно:

```
f[xᵢ] = yᵢ
f[xᵢ,...,xⱼ] = (f[xᵢ₊₁,...,xⱼ] - f[xᵢ,...,xⱼ₋₁]) / (xⱼ - xᵢ)
```

**Окно данных:** Используются последние `n` точек из буфера БЕЗ последней точки буфера (по умолчанию `n=4`). Это означает, что для работы алгоритма требуется минимум `n+1` точек в буфере.

**Особенности:**
- Расчет начинается после получения `n+1` точек
- Окно формируется как последние `n` точек без последней точки буфера
- Для каждого нового окна генерируются промежуточные точки с заданным шагом от первой до последней точки окна
- При EOF используется полный буфер (последние `n` точек) для финальной обработки
- При смене окна, если `last-x` находится вне диапазона нового окна, он игнорируется, и обработка начинается с начала нового окна

## Ключевые элементы реализации

### Архитектура алгоритмов

Используется **map-based подход**, где каждый алгоритм представлен как map с функциями. Это делает код более компактным и функциональным.

#### Создание алгоритма линейной интерполяции

```clojure
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
   :algorithm-name (constantly "linear")})
```

Каждый алгоритм содержит функции для работы с окном данных, обработки и очистки буфера.

#### Создание алгоритма Ньютона

```clojure
(defn create-newton-algorithm [n]
  "Создает алгоритм интерполяции Ньютона"
  {:window-size (constantly n)
   :can-process? #(>= (count %) (inc n))
   :get-window #(vec (take-last n (butlast %)))
   :cleanup-buffer (fn [buffer last-x]
                     "Удаляет уже обработанные точки, сохраняя минимум n+1 точек"
                     (if last-x
                       (let [min-required (inc n)
                             window (take-last n (butlast buffer))
                             window-start-x (first (first window))
                             window-end-x (first (last window))]
                         (if (>= last-x window-end-x)
                           (let [cleaned (vec (drop-while #(<= (first %) window-start-x) buffer))]
                             (if (>= (count cleaned) min-required)
                               cleaned
                               (vec (take-last min-required buffer))))
                           buffer))
                       buffer))
   :process (fn [_ window step last-x]
              (let [xs (compute-newton-points window step last-x)]
                (when (seq xs)
                  {:pts (mapv (fn [x] [x (newton-interp window x)]) xs)
                   :last-x (last xs)})))
   :algorithm-name (constantly "newton")})
```

### Основные функции

#### Парсинг входных данных

```clojure
(defn parse-line [s]
  "Парсит строку с координатами в форматах x;y, x\ty или x y"
  (try
    (let [[a b] (-> s str/trim
                    (str/replace ";" " ")
                    (str/replace "\t" " ")
                    (str/split #"\s+"))]
      [(Double/parseDouble a) (Double/parseDouble b)])
    (catch Exception _ nil)))
```

Функция обрабатывает строки в форматах `x;y`, `x\ty`, `x y`, преобразуя их в пары чисел.

#### Проверка сортировки точек

```clojure
(defn is-sorted? [buffer new-point]
  "Проверяет, что новая точка не нарушает сортировку по x"
  (if (empty? buffer)
    true
    (let [last-x (first (last buffer))
          new-x (first new-point)]
      (>= new-x last-x))))
```

Проверяет, что входящие точки отсортированы по возрастанию x. При нарушении сортировки выводится сообщение об ошибке, и точка пропускается.

#### Линейная интерполяция

```clojure
(defn linear-interp [[x1 y1] [x2 y2] x]
  "Вычисляет значение y для точки x между двумя заданными точками"
  (if (= x1 x2)
    y1
    (+ y1 (* (- y2 y1) (/ (- x x1) (- x2 x1))))))
```

Вычисляет значение `y` для точки `x` между двумя заданными точками.

```clojure
(defn gen-xs [start end step last-x]
  "Генерирует последовательность точек x с заданным шагом"
  (let [start (if last-x
                (max start (+ last-x step))
                start)]
    (when (< start end)
      (->> (iterate #(+ % step) start)
           (take-while #(<= % end))
           vec))))
```

Генерирует последовательность точек `x` с заданным шагом, начиная с `start` (или с `last-x + step`, если `last-x` задан) до `end`.

#### Интерполяция методом Ньютона

```clojure
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
```

Вычисляет разделенные разности для набора точек, используя хвостовую рекурсию.

```clojure
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
```

Вычисляет значение полинома Ньютона для точки `x`, используя предвычисленные разделенные разности.

#### Потоковая обработка

```clojure
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
          (do
            (println "ERROR: Point violates x sorting order. It'll be skipped.")
            (recur buffer last-states)))
        (recur buffer last-states))
      (do
        (process-algorithms-final algorithms buffer step last-states)
        (System/exit 0)))))
```

Основной цикл программы:
- Парсит аргументы командной строки
- Читает строки из стандартного ввода
- Проверяет сортировку точек по x
- Обновляет буфер точек
- Вызывает соответствующие алгоритмы интерполяции при наличии достаточного количества данных
- Отслеживает последнюю выведенную `x` для каждого алгоритма, чтобы избежать дублирования
- Очищает буфер от уже обработанных точек для оптимизации памяти
- При EOF выполняет финальный проход для вывода оставшихся точек

### Особенности реализации

1. **Map-based подход:** Алгоритмы представлены как map'ы с функциями, что делает код более компактным и функциональным.

2. **Оптимизация памяти:** Реализована очистка буфера от уже обработанных точек. Для линейной интерполяции удаляются точки до последней точки окна, для Ньютона гарантируется сохранение минимума n+1 точек.

3. **Неизменяемость данных:** Все операции создают новые структуры данных, не изменяя существующие.

4. **Ленивые последовательности:** Использование `iterate` и `take-while` для генерации точек позволяет эффективно работать с большими диапазонами.

5. **Хвостовая рекурсия:** Функции `divided-diffs` и `newton-interp` используют `recur` для оптимизации стека вызовов.

6. **Функции высшего порядка:** Использование `mapv`, `filter`, `doseq`, `reduce` для обработки коллекций.

7. **Управление состоянием:** Состояние программы (буфер точек, последние выведенные `x`) передается через параметры цикла `loop-recur`.

## Ввод/вывод программы

### Пример 1: Линейная интерполяция

**Команда:**
```bash
lein run --linear --step 0.7
```

**Ввод/вывод:**
```
< 0 0
< 1 1
> linear: 0 0
> linear: 0.7 0.7
< 2 2
> linear: 1.4 1.4
> 3 3
> linear: 2.1.4 2.1
> linear: 2.8 2.8
< EOF
> linear: 2.8 2.8
```

### Пример 2: Интерполяция методом Ньютона

**Команда:**
```bash
lein run --newton -n 4 --step 0.5
```

**Ввод/вывод:**
```
< 0 0
< 1 1
< 2 2
> 3 3
> 4 4
> newton: 0.0 0.0
> newton: 0.5 0.5
> newton: 1.0 1.0
> newton: 1.5 1.5
> newton: 2.0 2.0
> newton: 2.5 2.5
> newton: 3.0 3.0
< 5 5
> newton: 3.5 3.5
> newton: 4.0 4.0
< 7 7
> newton: 4.5 4.5
> newton: 5.0 5.0
< 8 8
> newton: 5.5 5.5
> newton: 6.0 6.0
> newton: 6.5 6.5
> newton: 7.0 7.0
< EOF
> newton: 7.5 7.5
> newton: 8.0 8.0
```

### Пример 3: Оба алгоритма одновременно

**Команда:**
```bash
lein run --linear --newton --step 0.5 -n 3
```

**Ввод/вывод:**
```
< 0 0
< 1 1
> linear: 0.0 0.0
> linear: 0.5 0.5
> linear: 1.0 1.0
< 2 2
> linear: 1.5 1.5
> linear: 2.0 2.0
< 3 3
> linear: 2.5 2.5
> linear: 3.0 3.0
> newton: 0.0 0.0
> newton: 0.5 0.5
> newton: 1.0 1.0
> newton: 1.5 1.5
> newton: 2.0 2.0
< 4 4
> linear: 3.5 3.5
> linear: 4.0 4.0
> newton: 2.5 2.5
> newton: 3.0 3.0
< EOF
> linear: 4.0 4.0
> newton: 3.5 3.5
> newton: 4.0 4.0
```

## Тестирование

Реализованы unit-тесты для проверки корректности работы алгоритмов:

```clojure
(deftest test-linear-interp
  (testing "Линейная интерполяция между двумя точками"
    (is (= 0.5 (linear-interp [0 0] [1 1] 0.5)))
    (is (= 1.5 (linear-interp [1 1] [2 2] 1.5)))))

(deftest test-newton-interp
  (testing "Интерполяция Ньютона для набора точек"
    (let [points [[0 0] [1 1] [2 4] [3 9]]]
      (is (= 0.0 (newton-interp points 0.0)))
      (is (= 1.0 (newton-interp points 1.0))))))

(deftest test-parse-line
  (testing "Парсинг строк с координатами"
    (is (= [0.0 0.0] (parse-line "0 0")))
    (is (= [1.0 2.0] (parse-line "1;2")))
    (is (= [1.0 2.0] (parse-line "1\t2")))))

(deftest test-linear-algorithm
  (testing "Алгоритм линейной интерполяции"
    (let [algorithm (create-linear-algorithm)]
      (is (= 2 ((:window-size algorithm))))
      (is (= "linear" ((:algorithm-name algorithm))))
      (is (true? ((:can-process? algorithm) [[0 0] [1 1]]))))))

(deftest test-newton-algorithm
  (testing "Алгоритм интерполяции Ньютона"
    (let [algorithm (create-newton-algorithm 4)]
      (is (= 4 ((:window-size algorithm))))
      (is (= "newton" ((:algorithm-name algorithm))))))
```

Все тесты проходят успешно (11 тестов).

## Выводы

В ходе лабораторной работы была реализована программа для интерполяции данных. Сначала задаются параметры для выбора алгоритма, шага интерполяции и количества задействованных точек (только для интерполяции алгоритмом Ньютона), затем выполняется расчет результата в потоковом режиме: приложение ожидает ввод, пока не получит достаточное количество данных для расчета, далее выводит промежуточные результаты и сдвигает окно расчета при добавлении новых точек.

Использованы ленивые последовательности: `iterate` и `take-while` для генерации точек интерполяции позволяют работать с большими диапазонами, не создавая промежуточные коллекции в памяти. Сохранен принцип неизменяемости переменных, характерный для функционального программирования: все операции создают новые структуры данных, что упрощает отслеживание состояния и делает код более предсказуемым. Буфер точек и состояние программы передаются через параметры цикла `loop-recur`, а не через изменяемые переменные. Используются характерные для clojure функции высшего порядка обработки коллекций: `mapv`, `filter`, `doseq`. Там, где это удобно, понятность кода достигается с применением макросов для построения цепочек обработки. Тестирование облегчалось засчет отдельной отладки каждой из выделенных функций.
