(ns my-app.views.staffing
  (:require
   [re-frame.core :as rf]
   [my-app.events :as events]
   [my-app.subs :as subs]
   [my-app.domain.periods :as periods-domain]
   [my-app.views.worktime :as worktime]))

;; -----------------------------
;; 1️⃣ Чистые функции для вычислений
;; -----------------------------

(defn percent-of-8 [hours]
  "Вычисляет значение в процентах от 8 часов"
  (if (number? hours)
    (/ hours 8)
    0))

(defn total-work-hours [position-id work-rows period-id]
  "Сумма трудозатрат по всем операциям данной должности за выбранный период"
  (reduce + (map #(or (:total-workload %) 0)
                 (filter #(= (:position-id %) position-id) work-rows))))

(defn net-norm
  [position-id worktime-table period-id non-norm break unavoidable]
  "Норма часов за период за вычетом процентов"
  (let [base-norm (get-in worktime-table [position-id period-id :total] 0)
        reduction (* base-norm (+ (percent-of-8 non-norm)
                                   (percent-of-8 break)
                                   (percent-of-8 unavoidable)))]
    (- base-norm reduction)))

(defn target-count [work-hours net-norm]
  (if (zero? net-norm)
    0
    (/ work-hours net-norm)))

(defn target-count-rounded [tc]
  (js/Math.round tc))

(defn diff-count [target actual]
  (js/Math.abs (- target actual)))

(defn actual-load [work-hours net-norm actual-count]
  (if (or (zero? net-norm) (zero? work-hours) (zero? actual-count))
    0
    (/ work-hours (* net-norm actual-count))))

(defn target-load [target-count rounded-target]
  (if (zero? rounded-target)
    0
    (/ target-count rounded-target)))

;; -----------------------------
;; 2️⃣ Компонент выбора периода
;; -----------------------------

(defn period-selector []
  (let [selected-period @(rf/subscribe [:calc-period])
        all-periods @(rf/subscribe [:periods])]
    [:div {:style {:margin-bottom "10px"}}
     [:label "Выберите расчетный период: "]
     [:select {:value selected-period
               :on-change #(rf/dispatch [:set-calc-period (.. % -target -value)])}
      (for [p all-periods]
        ^{:key (:id p)}
        [:option {:value (:id p)} (:label p)])]]))

;; -----------------------------
;; 3️⃣ Компонент строки таблицы
;; -----------------------------

(defn staffing-row [position work-rows worktime-table period-id]
  (let [position-id (:id position)
        position-name (:name position)
        ;; вводимые значения
        non-norm (:non-norm-hours position 0)
        break (:break-hours position 0)
        unavoidable (:unavoidable-break position 0)
        ;; вычисляемые
        non-norm-percent (percent-of-8 non-norm)
        break-percent (percent-of-8 break)
        unavoidable-percent (percent-of-8 unavoidable)
        total-hours (total-work-hours position-id work-rows period-id)
        net (net-norm position-id worktime-table period-id non-norm break unavoidable)
        vacation-days (get-in worktime-table [position-id period-id :vacation] 0)
        vacation-indicator (if (> vacation-days 0) "да" "-")
        tgt (target-count total-hours net)
        tgt-rounded (target-count-rounded tgt)
        actual (:count position 0)
        diff (diff-count tgt actual)
        act-load (actual-load total-hours net actual)
        tgt-load (target-load tgt tgt-rounded)]
    [:tr
     [:td position-name]
     ;; Ненормируемые операции
     [:td [:input {:type "number" :value non-norm
                   :on-change #(rf/dispatch [:update-staffing position-id :non-norm-hours (js/parseFloat (.. % -target -value))])}]]
     [:td (when non-norm-percent (.toFixed non-norm-percent 2))]
     ;; Время на отдых
     [:td [:input {:type "number" :value break
                   :on-change #(rf/dispatch [:update-staffing position-id :break-hours (js/parseFloat (.. % -target -value))])}]]
     [:td (when break-percent (.toFixed break-percent 2))]
     ;; Неустранимые перерывы
     [:td [:input {:type "number" :value unavoidable
                   :on-change #(rf/dispatch [:update-staffing position-id :unavoidable-break (js/parseFloat (.. % -target -value))])}]]
     [:td (when unavoidable-percent (.toFixed unavoidable-percent 2))]
     ;; Расчетный период
     [:td (str period-id)]
     ;; Рабочих часов за период
     [:td total-hours]
     ;; Норма часов за период за вычетом отпуска
     [:td (js/Math.round net)]
     ;; Кол-во дней отпуска
     [:td vacation-days]
     [:td vacation-indicator]
     ;; Целевая численность
     [:td (.toFixed tgt 1)]
     [:td tgt-rounded]
     ;; Фактическая численность
     [:td actual]
     ;; Разница
     [:td diff]
     ;; Фактическая загруженность
     [:td (.toFixed act-load 2)]
     ;; Целевая загруженность
     [:td (.toFixed tgt-load 2)]]))

;; -----------------------------
;; 4️⃣ Главный компонент страницы
;; -----------------------------

(defn staffing-page []
  (let [positions @(rf/subscribe [:positions])
        work-rows @(rf/subscribe [:work])
        worktime-table @(rf/subscribe [:worktime-table])
        selected-period @(rf/subscribe [:calc-period])]
    [:div
     [:h2 "Расчет численности"]
     [period-selector]
     [:table {:border 1 :cellPadding 6}
      [:thead
       [:tr
        [:th "Должность"]
        [:th "Ненормирумые операции, ПЗР, ОРМ"]
        [:th "Ненормирумые %, /8"]
        [:th "Время на отдых и личные надобности"]
        [:th "Время на отдых %, /8"]
        [:th "Неустранимые перерывы в часах"]
        [:th "Неустранимые %, /8"]
        [:th "Расчетный период"]
        [:th "Рабочих часов за период"]
        [:th "Норма часов за период за вычетом отпуска"]
        [:th "Количество дней отпуска"]
        [:th "Отпуск"]
        [:th "Целевая численность"]
        [:th "Целевая численность округленная"]
        [:th "Фактическая численность"]
        [:th "Разница между целевой и фактической"]
        [:th "Фактическая загруженность"]
        [:th "Целевая загруженность"]]]
      [:tbody
       (for [pos positions]
         ^{:key (:id pos)} [staffing-row pos work-rows worktime-table selected-period])]]
     ;; -----------------------------
     ;; 5️⃣ Краткий вывод о состоянии кадрового делопроизводства
     ;; -----------------------------
     [:div {:style {:margin-top "20px"}}
      (let [total-diff (reduce + (map #(diff-count
                                        (target-count
                                         (total-work-hours (:id %) work-rows selected-period)
                                         (net-norm (:id %) worktime-table selected-period
                                                   (:non-norm-hours % 0)
                                                   (:break-hours % 0)
                                                   (:unavoidable-break % 0)))
                                        (:count % 0))
                                     positions))
            avg-load (if (empty? positions) 0
                       (/ (reduce + (map #(actual-load
                                           (total-work-hours (:id %) work-rows selected-period)
                                           (net-norm (:id %) worktime-table selected-period
                                                     (:non-norm-hours % 0)
                                                     (:break-hours % 0)
                                                     (:unavoidable-break % 0))
                                           (:count % 0))
                                         positions))
                          (count positions)))]
        [:div
         [:p (str "Общее отклонение между целевой и фактической численностью: " total-diff)]
         [:p (str "Средняя фактическая загруженность по компании: " (.toFixed avg-load 2))]])]]))
