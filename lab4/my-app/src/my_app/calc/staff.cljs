(ns my-app.calc.staff)

;; сумма трудозатрат по должности
(defn workload-by-position
  [works position-id selected-period]
  (->> works
       ;; 1. только нужная должность
       (filter #(= (:position-id %) position-id))

       ;; 2. только выбранный расчетный период
       (filter #(= (:calculation-period %) selected-period))

       ;; 3. берём рассчитанные трудозатраты
       ;;    (manual-hours уже должен учитывать период)
       (map :manual-hours)

       ;; 4. убираем пустые
       (remove nil?)

       ;; 5. суммируем
       (reduce + 0)))


(defn total-percent-loss
  [{:keys [non-norm-hours rest-hours break-hours]}]
  (+ (/ non-norm-hours 8)
     (/ rest-hours 8)
     (/ break-hours 8)))

(defn effective-period-norm
  [raw-norm inputs]
  (let [loss (total-percent-loss inputs)]
    (* raw-norm (- 1 loss))))


(defn percent-of-day [hours]
  (/ (or hours 0) 8))

(defn effective-norm
  [raw-norm percent-loss]
  (* (or raw-norm 0) (- 1 percent-loss)))

(defn target-staff
  [work-hours effective-norm]
  (if (zero? effective-norm)
    0
    (/ work-hours effective-norm)))

(defn factual-load
  [work norm factual]
  (if (or (zero? work) (zero? norm) (zero? factual))
    0
    (/ work (* norm factual))))

(defn target-load
  [target rounded]
  (if (zero? rounded)
    0
    (/ target rounded)))
