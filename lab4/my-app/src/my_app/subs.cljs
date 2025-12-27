(ns my-app.subs
  (:require
   [re-frame.core :as rf]
   [my-app.domain.periods :refer [periods]]))

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

;; подписка на выбранный период
(rf/reg-sub
 :calc-period
 (fn [db _] (:calc-period db)))

;; подписка на список периодов для выбора
(rf/reg-sub
 :periods
 (fn [_ _] my-app.domain.periods/periods))

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
     (into {}  ;; <-- map вместо вектора
           (for [p positions
                 period periods]
             (let [{:keys [id type months]} period
                   vacation (if (= type :month)
                              (get-in worktime [(:id p) id :vacation] 0)
                              (reduce + (map #(get-in worktime [(:id p) % :vacation] 0) months)))
                   norm (if (= type :month)
                          (get norm-hours id 0)
                          (reduce + (map norm-hours months)))
                   total (- norm (* vacation 8))]
               [[(:id p) id]
                {:position-name (:name p)
                 :position-id (:id p)
                 :period-id id
                 :period-label (:label period)
                 :type type
                 :norm norm
                 :vacation vacation
                 :total total}]))))))


