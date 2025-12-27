(ns my-app.calc.calendar)

;; Количество рабочих дней в 2025 году по месяцам
(def working-days
  {:jan 21 :feb 20 :mar 21 :apr 22 :may 20 :jun 21
   :jul 23 :aug 21 :sep 22 :oct 23 :nov 21 :dec 21})

;; Количество рабочих дней в кварталах, полугодиях и году
(def periods
  {:q1 [:jan :feb :mar]
   :q2 [:apr :may :jun]
   :q3 [:jul :aug :sep]
   :q4 [:oct :nov :dec]
   :h1 [:jan :feb :mar :apr :may :jun]
   :h2 [:jul :aug :sep :oct :nov :dec]
   :year [:jan :feb :mar :apr :may :jun
          :jul :aug :sep :oct :nov :dec]})

(defn norm-for-period
  "Вычисляет норму рабочего времени для любого периода на основе годовой нормы и рабочих дней."
  [year-norm period-id]
  (let [days-in-year (reduce + (vals working-days))
        months (cond
                 (contains? working-days period-id) [period-id]  ;; месяц
                 :else (get periods period-id))]                ;; квартал, полугодие, год
    (if months
      (let [days-in-period (reduce + (map working-days months))]
        (* year-norm (/ days-in-period days-in-year)))
      0)))

