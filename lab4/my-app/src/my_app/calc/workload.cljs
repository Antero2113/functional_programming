(ns my-app.calc.workload)

(defn avg-time [min-time max-time]
  (if (and (number? min-time) (number? max-time))
    (/ (+ (* 3 min-time) (* 2 max-time)) 5)
    0))

(defn to-hours [value unit]
  (if (number? value)
    (case unit
      "минуты" (/ value 60)
      "часы" value
      0)
    0))

(defn get-position-norm
  [position-id positions]
  (let [pos (some #(when (= (:id %) position-id) %) positions)]
    (or (:norm pos) 0)))

(def period->multiplier
  {:month   {:день 21 :неделя 4.3 :месяц 1}   
   :quarter {:день 63 :неделя 13  :месяц 3 :квартал 1}
   :half    {:день 126 :неделя 26 :месяц 6 :квартал 2 :полгода 1}
   :year    {:день 247 :неделя 52 :месяц 12 :квартал 4 :год 1}})

(def period-type
  {:jan :month :feb :month :mar :month :apr :month :may :month :jun :month
   :jul :month :aug :month :sep :month :oct :month :nov :month :dec :month
   :q1  :quarter :q2 :quarter :q3 :quarter :q4 :quarter
   :h1  :half :h2 :half
   :year :year})

(defn frequency-multiplier [frequency-param selected-period]
  (let [ptype (get period-type selected-period)]
    (get-in period->multiplier
            [ptype (keyword frequency-param)]
            0)))


(defn workload
  [row position-norm selected-period]
  (let [{:keys [min-time max-time time-unit frequency frequency-param
                manual-hours full-period proportional-period]} row
        frequency (or frequency 0)
        avg       (avg-time min-time max-time)
        hours     (to-hours avg time-unit)
        mult      (frequency-multiplier frequency-param selected-period)
        base-calc (* hours frequency mult)]
    
    (cond
      ;; 1. пропорциональный расчет: используем manual-hours если задан
      proportional-period
      (if (number? manual-hours)
        manual-hours
        base-calc)

      ;; 2. полный расчет: всегда через формулу
      full-period
      base-calc

      ;; 3. ручной ввод
      (number? manual-hours)
      manual-hours

      ;; 4. обычный расчет
      :else
      base-calc)))
