(ns my-app.views.workdescription
  (:require
   [re-frame.core :as rf]
   [my-app.subs :as subs]
   [my-app.events :as events]
   [my-app.calc.workload :as workload]))

(defn work-row [row positions]
  (let [{:keys [id position-id business-process operation time-unit
                min-time max-time frequency-param frequency
                manual-hours full-period proportional-period
                calculation-period auto-hours manual-value]} row
        period @(rf/subscribe [:selected-period])
        avg (workload/avg-time min-time max-time)
        position-norm (workload/get-position-norm position-id positions)
        calculated (workload/workload row position-norm period)]
    [:tr
     ;; Должность
     [:td
      [:select {:value (or position-id "")
                :on-change #(rf/dispatch
                             [::events/update-work id :position-id
                              (js/parseInt (.. % -target -value))])}
       [:option {:value ""} "--"]
       (for [p positions]
         ^{:key (:id p)}
         [:option {:value (:id p)} (:name p)])]]

     ;; Бизнес-процесс
     [:td [:input {:value business-process
                   :on-change #(rf/dispatch
                                [::events/update-work id :business-process
                                 (.. % -target -value)])}]]

     ;; Операция
     [:td [:input {:value operation
                   :on-change #(rf/dispatch
                                [::events/update-work id :operation
                                 (.. % -target -value)])}]]

     ;; Единицы времени
     [:td
      [:select {:value time-unit
                :on-change #(rf/dispatch
                             [::events/update-work id :time-unit
                              (.. % -target -value)])}
       [:option {:value "минуты"} "минуты"]
       [:option {:value "часы"} "часы"]]]

     ;; min / max
     [:td [:input {:type "number" :value min-time
                   :on-change #(rf/dispatch
                                [::events/update-work id :min-time
                                 (js/parseFloat (.. % -target -value))])}]]
     [:td [:input {:type "number" :value max-time
                   :on-change #(rf/dispatch
                                [::events/update-work id :max-time
                                 (js/parseFloat (.. % -target -value))])}]]

     ;; среднее
     [:td (when avg (.toFixed avg 1))]

     ;; параметр частоты
     [:td
      [:select {:value frequency-param
                :on-change #(rf/dispatch
                             [::events/update-work id :frequency-param
                              (.. % -target -value)])}
       [:option {:value "день"} "день"]
       [:option {:value "неделя"} "неделя"]
       [:option {:value "месяц"} "месяц"]
       [:option {:value "квартал"} "квартал"]
       [:option {:value "полгода"} "полгода"]
       [:option {:value "год"} "год"]]]

     ;; частота
     [:td [:input {:type "number" :value frequency
                   :on-change #(rf/dispatch
                                [::events/update-work id :frequency
                                 (js/parseFloat (.. % -target -value))])}]]

     ;; произвольные часы
     [:td [:input {:type "number" :value manual-hours
                   :on-change #(rf/dispatch
                                [::events/set-manual-hours id
                                 (js/parseFloat (.. % -target -value))])}]]

     ;; учесть полностью
     [:td [:input {:type "checkbox" :checked full-period
                   :on-change #(rf/dispatch
                                [::events/set-full-period id])}]]

     ;; пропорционально
     [:td [:input {:type "checkbox" :checked proportional-period
                   :on-change #(rf/dispatch
                                [::events/set-proportional id])}]]

     ;; период
     [:td (name period)]


     ;; норма должности (read-only)
     [:td position-norm]

     [:td (when calculated (.toFixed calculated 2))]

     ;; удалить
     [:td [:button {:on-click #(rf/dispatch [::events/remove-work id])}
           "✖"]]]))

(defn workdescription-page []
  (let [rows @(rf/subscribe [:work])
        positions @(rf/subscribe [:positions])
        period @(rf/subscribe [:selected-period])]
    [:div
     [:h2 "Описание работ"]
     [:button {:on-click #(rf/dispatch [::events/add-work])}
      "Добавить работу"]
     [:table {:border 1 :cellPadding 4}
      [:thead
       [:tr
        [:th "Должность"]
        [:th "Бизнес-процесс"]
        [:th "Операция"]
        [:th "Ед.вр."]
        [:th "min"]
        [:th "max"]
        [:th "Hв"]
        [:th "Период"]
        [:th "Частота"]
        [:th "Произв. часы"]
        [:th "Полностью"]
        [:th "Пропорц."]
        [:th "Расчетный период"]
        [:th "Норма рабочего времени"]
        [:th "Трудозатраты за период"]
        [:th ""]]]
      [:tbody
       (for [r rows]
         ^{:key (:id r)} [work-row r positions])]]]))
