(ns my-app.views.worktime
  (:require
   [re-frame.core :as rf]
   [my-app.events :as events]
   [my-app.calc.calendar :as calendar]))

(defn row [{:keys [position-name position-id period-id period-label
                   type vacation]} positions]
  (let [year-norm (some #(when (= (:id %) position-id) (:norm %)) positions)
        norm (calendar/norm-for-period (or year-norm 0) period-id)
        total (- norm (* (or vacation 0) 8))
        norm-display (when norm (.toFixed norm 1))
        total-display (when total (.toFixed total 1))]
    [:tr
     [:td position-name]
     [:td period-label]
     [:td norm-display]  
     [:td
      (if (or (= type :month) (nil? type))
        [:input {:type "number"
                 :min 0
                 :value (or vacation 0)
                 :on-change #(rf/dispatch
                              [::events/set-vacation-days
                               position-id period-id
                               (js/parseInt (.. % -target -value))])}]
        vacation)]
     [:td total-display]]))  


(defn worktime-page []
  (let [rows @(rf/subscribe [:worktime-table])
        positions @(rf/subscribe [:positions])]
    [:div
     [:h2 "Рабочее время"]
     [:table {:border 1 :cellPadding 6}
      [:thead
       [:tr
        [:th "Должность"]
        [:th "Расчетный период"]
        [:th "Норма рабочего времени"]
        [:th "Количество дней отпуска"]
        [:th "Итого норма времени"]]]
      [:tbody
       (for [r rows]
         ^{:key (str (:position-id r) "-" (:period-id r))}
         [row r positions])]]]))
