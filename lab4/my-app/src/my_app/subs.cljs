(ns my-app.subs
  (:require
   [re-frame.core :as rf]
   [my-app.domain.periods :refer [periods]]
   [my-app.calc.staff :as staff]
   [my-app.calc.workload :as workload]))

(rf/reg-sub
 :active-page
 (fn [db _]
   (:active-page db)))

(rf/reg-sub
 :positions
 (fn [db _]
   (:positions db)))

(rf/reg-sub
 :work
 (fn [db _]
   (:work db)))

;; ДЛЯ РАБОЧЕГО ВРЕМЕНИ

(def norm-hours
  {:jan 168 :feb 160 :mar 168 :apr 168 :may 160 :jun 168
   :jul 168 :aug 168 :sep 168 :oct 176 :nov 168 :dec 176})

(defn sum-vacation [worktime position-id months]
  (reduce
   (fn [acc m]
     (+ acc (get-in worktime [position-id m :vacation-days] 0)))
   0
   months))

(rf/reg-sub
 :worktime-table
 (fn [db _]
   (let [positions (:positions db)
         worktime (:worktime db)]
     (for [p positions
           period periods]
       (let [{:keys [id type months]} period
             vacation (if (= type :month)
                        (get-in worktime [(:id p) id :vacation-days] 0)
                        (sum-vacation worktime (:id p) months))
             norm (if (= type :month)
                    (get norm-hours id 0)
                    (reduce + (map norm-hours months)))
             total (- norm (* vacation 8))]
         {:position-name (:name p)
          :position-id (:id p)
          :period-id id
          :period-label (:label period)
          :type type
          :norm norm
          :vacation vacation
          :total total})))))

;; РАСЧЕТ ЧИСЛЕННОСТИ

(rf/reg-sub
 :selected-period
 (fn [db _]
   (:selected-period db)))


(rf/reg-event-db
 :set-selected-period
 (fn [db [_ period]]
   (assoc db :selected-period period)))


(rf/reg-sub
 :staffing-table
 :<- [:positions]
 :<- [:work]
 :<- [:worktime-table]
 :<- [:selected-period]
 :<- [:staffing-input]
 (fn [[positions work worktime period-id _] _]
   (for [{:keys [id name count]} positions]
     (let [inputs @(rf/subscribe [:staffing-input id])
           period-row (some #(when (= (:period-id %) period-id) %) worktime)

           raw-norm (:total period-row)
           vacation (:vacation period-row)

           work-hours (staff/workload-by-position work id period-id)
           effective-norm (staff/effective-period-norm raw-norm inputs)

           target (if (zero? effective-norm)
                    0
                    (/ work-hours effective-norm))

           rounded (js/Math.round target)]

       {:position name
        :inputs inputs

        :period (:period-label period-row)
        :work-hours work-hours
        :effective-norm effective-norm

        :vacation vacation
        :vacation-flag (pos? vacation)

        :target target
        :rounded rounded
        :factual count
        :diff (js/Math.abs (- rounded count))

        :fact-load (staff/factual-load work-hours effective-norm count)
        :target-load (staff/target-load target rounded)}))))


(rf/reg-sub
 :staffing-input
 (fn [db [_ position-id]]
   (get-in db [:staffing-inputs position-id]
           {:non-norm-hours 0
            :rest-hours 0
            :break-hours 0})))

(rf/reg-sub
 :workload-by-position
 :<- [:work]
 :<- [:positions]
 :<- [:selected-period]   
 (fn [[work positions selected-period] [_ position-id]]
   (let [norm (some #(when (= (:id %) position-id) (:norm %)) positions)]
     (->> work
          (filter #(= (:position-id %) position-id))
          (map #(workload/workload % norm selected-period)) 
          (remove nil?)
          (reduce + 0)))))

