(ns my-app.db)

(def default-db
  {:active-page :home

   :positions
   [{:id 1 :name "Начальник отдела" :count 1 :norm 1800}]

   :work []
   
   :worktime{1 {:jan {:total 160 :vacation 5}}} 
   
   :calc-period :year})


