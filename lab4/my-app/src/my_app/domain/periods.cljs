(ns my-app.domain.periods)

(def periods
  [{:id :jan :label "Январь" :type :month}
   {:id :feb :label "Февраль" :type :month}
   {:id :mar :label "Март" :type :month}
   {:id :apr :label "Апрель" :type :month}
   {:id :may :label "Май" :type :month}
   {:id :jun :label "Июнь" :type :month}
   {:id :jul :label "Июль" :type :month}
   {:id :aug :label "Август" :type :month}
   {:id :sep :label "Сентябрь" :type :month}
   {:id :oct :label "Октябрь" :type :month}
   {:id :nov :label "Ноябрь" :type :month}
   {:id :dec :label "Декабрь" :type :month}

   {:id :q1 :label "I квартал" :type :quarter :months [:jan :feb :mar]}
   {:id :q2 :label "II квартал" :type :quarter :months [:apr :may :jun]}
   {:id :q3 :label "III квартал" :type :quarter :months [:jul :aug :sep]}
   {:id :q4 :label "IV квартал" :type :quarter :months [:oct :nov :dec]}

   {:id :h1 :label "I полугодие" :type :half :months [:jan :feb :mar :apr :may :jun]}
   {:id :h2 :label "II полугодие" :type :half :months [:jul :aug :sep :oct :nov :dec]}

   {:id :year :label "2022 год" :type :year
    :months [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]}])
