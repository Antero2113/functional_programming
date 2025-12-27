(ns my-app.views
  (:require
   [re-frame.core :as rf]
   [my-app.subs :as subs]
   [my-app.views.common :as common]
   [my-app.views.home :as home]
   [my-app.views.positions :as positions]
   [my-app.views.workdescription :as workdescription]
   [my-app.views.worktime :as worktime]
   [my-app.views.staffing :as staffing]))

(defn main-panel []
  (let [page @(rf/subscribe [:active-page])]
    [:div
     [common/tabs-panel]
     [:hr]
     (case page
       :home [home/home-page]
       :positions [positions/positions-page]
       :workdescription [workdescription/workdescription-page]
       :worktime [worktime/worktime-page]
       :staffing [staffing/staffing-page]
       [:div "Раздел в разработке"])]))
