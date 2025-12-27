(ns my-app.views.staffing
  (:require
   [re-frame.core :as rf]
   [my-app.events :as events]
   [my-app.domain.periods :refer [periods]]))

(defn period-selector []
  (let [selected @(rf/subscribe [:selected-period])]
    [:select
     {:value selected
      :on-change #(rf/dispatch
                   [:set-selected-period
                    (keyword (.. % -target -value))])}
     (for [{:keys [id label]} periods]
       ^{:key id}
       [:option {:value id} label])]))

(defn staffing-row [p period]
  (let [{:keys [id name count]} p
        inputs @(rf/subscribe [:staffing-input id])
        work-hours @(rf/subscribe [:workload-by-position id])
        rows @(rf/subscribe [:worktime-table])
        period-row (some #(when (= (:period-id %) period) %) rows)

        raw-norm (:total period-row)
        vacation (:vacation period-row)

        percent-loss (+ (/ (:non-norm-hours inputs) 8)
                         (/ (:rest-hours inputs) 8)
                         (/ (:break-hours inputs) 8))

        effective-norm (* raw-norm (- 1 percent-loss))

        target (if (zero? effective-norm)
                 0
                 (/ work-hours effective-norm))

        rounded (js/Math.round target)

        fact-load (if (or (zero? effective-norm)
                          (zero? work-hours)
                          (zero? count))
                    0
                    (/ work-hours (* effective-norm count)))

        target-load (if (zero? rounded) 0 (/ target rounded))]

    [:tr
     [:td name]

     ;; Ненормируемые
     [:td [:input {:type "number" :value (:non-norm-hours inputs)
                   :on-change #(rf/dispatch
                                [::events/set-staffing-input
                                 id :non-norm-hours
                                 (js/parseFloat (.. % -target -value))])}]]
     [:td (.toFixed (/ (:non-norm-hours inputs) 8) 2)]

     ;; Отдых
     [:td [:input {:type "number" :value (:rest-hours inputs)
                   :on-change #(rf/dispatch
                                [::events/set-staffing-input
                                 id :rest-hours
                                 (js/parseFloat (.. % -target -value))])}]]
     [:td (.toFixed (/ (:rest-hours inputs) 8) 2)]

     ;; Перерывы
     [:td [:input {:type "number" :value (:break-hours inputs)
                   :on-change #(rf/dispatch
                                [::events/set-staffing-input
                                 id :break-hours
                                 (js/parseFloat (.. % -target -value))])}]]
     [:td (.toFixed (/ (:break-hours inputs) 8) 2)]

     [:td (:period-label period-row)]
     [:td (.toFixed work-hours 2)]
     [:td (.toFixed effective-norm 2)]
     [:td vacation]
     [:td (if (pos? vacation) "Отпуск" "—")]
     [:td (.toFixed target 2)]
     [:td rounded]
     [:td count]
     [:td (js/Math.abs (- rounded count))]
     [:td (.toFixed fact-load 2)]
     [:td (.toFixed target-load 2)]]))

(defn staffing-page []
  (let [positions @(rf/subscribe [:positions])
        period @(rf/subscribe [:selected-period])]
    [:div
     [:h2 "Расчёт численности"]
     [:div "Период: " [period-selector]]
     [:table {:border 1 :cellPadding 4}
      [:thead
       [:tr
        [:th "Должность"]
        [:th "Ненорм. опер."]
        [:th "%"]
        [:th "Отдых"]
        [:th "%"]
        [:th "Перерывы"]
        [:th "%"]
        [:th "Период"]
        [:th "Рабочие часы"]
        [:th "Норма"]
        [:th "Дни отпуска"]
        [:th "Отпуск"]
        [:th "Целевая"]
        [:th "Округл."]
        [:th "Факт"]
        [:th "Разница"]
        [:th "Факт. загр."]
        [:th "Цел. загр."]]]
      [:tbody
       (for [p positions]
         ^{:key (:id p)}
         [staffing-row p period])]]]))
