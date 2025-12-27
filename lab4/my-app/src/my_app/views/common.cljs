(ns my-app.views.common
  (:require
   [re-frame.core :as rf]
   [my-app.events :as events]))

(def tabs
  [[:home "Главная"]
   [:positions "Штатные должности"]
   [:workdescription "Описание работ"]
   [:worktime "Рабочее время"]
   [:staffing "Расчет численности"]])


(defn tabs-panel []
  (let [active @(rf/subscribe [:active-page])]
    [:div.tabs
     (for [[id title] tabs]
       ^{:key id}
       [:button
        {:style {:margin-right "8px"
                 :font-weight (when (= id active) "bold")}
         :on-click #(rf/dispatch [:set-active-page id])}
        title])]))
