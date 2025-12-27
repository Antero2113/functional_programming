(ns my-app.events
  (:require
   [re-frame.core :as rf]
   [my-app.db :as db]))

(rf/reg-event-db
 :initialize-db
 (fn [_ _]
   db/default-db))

(rf/reg-event-db
 :set-active-page
 (fn [db [_ page]]
   (assoc db :active-page page)))


;; ШТАТНЫЕ ДОЛЖНОСТИ

;; обновление существующей должности
(rf/reg-event-db
 ::update-position
 (fn [db [_ id field value]]
   (update db :positions
           (fn [positions]
             (mapv
              (fn [p]
                (if (= (:id p) id)
                  (assoc p field value)
                  p))
              positions)))))

;; добавление новой должности
(rf/reg-event-db
 ::add-position
 (fn [db _]
   (let [new-id (->> (:positions db)
                     (map :id)
                     (apply max 0)
                     inc)
         new-pos {:id new-id
                  :name ""
                  :count 1}]
     (update db :positions conj new-pos))))

;; удаление должности
(rf/reg-event-db
 ::remove-position
 (fn [db [_ id]]
   (update db :positions
           (fn [positions]
             (vec (remove #(= (:id %) id) positions))))))

;; ОПИСАНИЕ РАБОТ

(rf/reg-event-db
 ::add-work
 (fn [db _]
   (let [id (inc (apply max 0 (map :id (:work db))))]
     (update db :work conj
             {:id id
              :position-id nil
              :business-process ""
              :operation ""
              :time-unit "минуты"
              :min-time nil
              :max-time nil
              :frequency-param "день"
              :frequency 1
              :manual-hours nil
              :full-period false
              :proportional-period true
              :calculation-period ""}))))

(rf/reg-event-db
 ::update-work
 (fn [db [_ id k v]]
   (update db :work
           #(mapv (fn [r]
                    (if (= (:id r) id) (assoc r k v) r))
                  %))))

(rf/reg-event-db
 ::remove-work
 (fn [db [_ id]]
   (update db :work #(vec (remove (fn [r] (= (:id r) id)) %)))))

;; взаимоисключение
(rf/reg-event-db
 ::set-manual-hours
 (fn [db [_ id v]]
   (update db :work
           #(mapv (fn [r]
                    (if (= (:id r) id)
                      (assoc r
                             :manual-hours v
                             :full-period false
                             :proportional-period false)
                      r))
                  %))))

(rf/reg-event-db
 ::set-full-period
 (fn [db [_ id]]
   (update db :work
           #(mapv (fn [r]
                    (if (= (:id r) id)
                      (assoc r
                             :full-period true
                             :manual-hours nil
                             :proportional-period false)
                      r))
                  %))))

(rf/reg-event-db
 ::set-proportional
 (fn [db [_ id]]
   (update db :work
           #(mapv (fn [r]
                    (if (= (:id r) id)
                      (assoc r
                             :proportional-period true
                             :manual-hours nil
                             :full-period false)
                      r))
                  %))))

;; РАБОЧЕЕ ВРЕМЯ

;; Ввод дней отпуска для месяцев
(rf/reg-event-db
 ::set-vacation-days
 (fn [db [_ position-id period-id days]]
   (assoc-in db [:worktime position-id period-id :vacation-days] days)))

;; РАСЧЕТ ЧИСЛЕННОСТИ

(rf/reg-event-db
 :update-staffing
 (fn [db [_ position-id field value]]
   (update db :positions
           (fn [positions]
             (mapv (fn [p]
                     (if (= (:id p) position-id)
                       (assoc p field value)
                       p))
                   positions)))))

(rf/reg-event-db
 :set-calc-period
 (fn [db [_ period-id]]
   (assoc db :calc-period period-id)))

(rf/reg-event-db
 :set-vacation-days
 (fn [db [_ position-id period-id value]]
   (assoc-in db [:worktime position-id period-id :vacation] value)))

