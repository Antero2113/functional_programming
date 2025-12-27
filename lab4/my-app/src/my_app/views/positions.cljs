(ns my-app.views.positions
  (:require
   [re-frame.core :as rf]
   [my-app.events :as events]
   [my-app.subs :as subs]))

(defn position-row [{:keys [id name count norm]}]
  [:tr
   [:td
    [:input {:value name
             :placeholder "Название должности"
             :on-change #(rf/dispatch
                          [::events/update-position id :name (.. % -target -value)])}]]
   [:td
    [:input {:type "number"
             :min 0
             :value count
             :on-change #(rf/dispatch
                          [::events/update-position id :count (js/parseInt (.. % -target -value))])}]]
   [:td
    [:input {:type "number"
             :min 0
             :value norm
             :on-change #(rf/dispatch
                          [::events/update-position id :norm (js/parseFloat (.. % -target -value))])}]]
   [:td
    [:button {:on-click #(rf/dispatch [::events/remove-position id])} "Удалить"]]])


(defn positions-page []
  (let [positions @(rf/subscribe [:positions])]
    [:div
     [:h2 "Штатные должности"]
     [:button {:on-click #(rf/dispatch [::events/add-position])}
      "Добавить должность"]
     [:table {:border 1 :cellPadding 6 :style {:margin-top "10px"}}
      [:thead
       [:tr
        [:th "Должность"]
        [:th "Количество сотрудников"]
        [:th "Норма"]
        [:th ""]]]
      [:tbody
       (for [p positions]
         ^{:key (:id p)} [position-row p])]]]))
