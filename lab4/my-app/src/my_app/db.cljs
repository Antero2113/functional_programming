(ns my-app.db)

(def default-db
  {:active-page :home
   :selected-period :year
   :positions
   [{:id 1 :name "Начальник отдела" :count 1 :norm 1800}]
   :work []
   :worktime{} 
   :staffing-inputs {}})


