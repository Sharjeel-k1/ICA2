(ns closure-projects.ICA2.clojure_analysis
  (:require
            [clojure.java.io :as io]
            [clojure.string :as str]))


;implementing split to read csv file
(defn fetch-csv-data [filename]
  (with-open [reader (io/reader filename)]
    (let [lines (line-seq reader)]
      (map #(apply hash-map (map keyword (str/split % #","))) lines))))

; Function to process rows into a more structured format
(defn parse-data-row [row]
  {:full-name (nth row :full-name)
   :yob (Integer/parseInt (nth row :yob))
   :departure (nth row :departure)
   :destination (nth row :destination)
   :pay (Double/parseDouble (nth row :pay))})

; Process CSV data
(defn process-csv-data [filename]
  (map parse-data-row (rest (fetch-csv-data filename)))) ; skip headers using rest


; Function to separate name into first name and surname
(defn separate-name [record]
  (let [[name surname] (str/split (:full-name record) #" ")]
    (assoc record :name name :surname surname)))

; Function to calculate personal age based on the current year
(defn calculate-personal-age [record current-year]
  (assoc record :age (- current-year (:yob record))))

; Utility function to identify family groups
(defn classify-family-group [transformed-data]
  (let [grouped-by-traits (group-by #(vector (:surname %) (:departure %) (:destination %) (:paid %)) transformed-data)]
    (mapcat (fn [[_ group]]
              (let [adults (filter #(>= (:age %) 18) group)
                    children (remove #(>= (:age %) 18) group)]
                (if (and (>= (count adults) 1) (>= (count children) 1))
                  (map #(assoc % :relation "family") group)
                  (map #(assoc % :relation "group") group))))
            grouped-by-traits)))

; Transforming data with updated functions
(defn transform-data [dataset current-year]
  (->> dataset
       (map #(-> % (separate-name) (calculate-personal-age current-year)))
       (classify-family-group)))

; Function to predict future sales
(defn predict-future-sales [transformed-data]
  (let [grouped-by-route (group-by #(vector (:departure %) (:destination %)) transformed-data)
        max-prices (->> grouped-by-route
                        (map (fn [[k v]] [k (apply max (map :paid v))]))
                        (into {}))
        max-price-flagged (map #(assoc % :max-price-sold (= (:paid %)
                                                            (get max-prices [(:departure %) (:destination %)])))
                               transformed-data)
        grouped-by-category (group-by #(vector (:relation %) (:departure %) (:destination %))
                                      max-price-flagged)]
    (->> grouped-by-category
         (map (fn [[k v]]
                (let [total (count v)
                      max-price-sold-count (count (filter :max-price-sold v))
                      success-rate (float (/ max-price-sold-count total))]
                  {:group-type (nth k 0)
                   :departure (nth k 1)
                   :destination (nth k 2)
                   :success-rate success-rate})))
         (into []))))

; Function to calculate purchase probabilities
(defn calculate-purchase-probabilities [transformed-data increase-percentage statistics success-rates future-sales-predictions]
  (let [increase-price (fn [paid] (+ paid (* paid increase-percentage 0.01)))
        with-new-max-price (map #(assoc % :new-max-price (increase-price (:paid %))) transformed-data)
        merge-data (fn [record]
                     (let [matching-stats (first (filter #(and (= (:group-type %) (:relation record))
                                                               (= (:departure %) (:departure record))
                                                               (= (:destination %) (:destination record)))
                                                         statistics))
                           matching-success-rates (first (filter #(and (= (:group-type %) (:relation record))
                                                                       (= (:departure %) (:departure record))
                                                                       (= (:destination %) (:destination record)))
                                                                 success-rates))
                           matching-future-sales (first (filter #(and (= (:group-type %) (:relation record))
                                                                      (= (:departure %) (:departure record))
                                                                      (= (:destination %) (:destination record)))
                                                                future-sales-predictions))]
                       (merge record matching-stats matching-success-rates matching-future-sales)))
        merged-data (map merge-data with-new-max-price)
        calculate-probability (fn [row]
                                (if (<= (:new-max-price row) (:max row))
                                  (min (* (:success-rate row) (:predicted-demand row)) 1)
                                  0))
        with-probability (map #(assoc % :probability-with-increase (calculate-probability %)) merged-data)
        filtered-data (filter #(> (:probability-with-increase %) 0) with-probability)]
    (distinct (map #(select-keys % [:group-type :departure :destination :probability-with-increase]) filtered-data))))

; Function to calculate mean
(defn compute-mean [values]
  (let [sum (reduce + 0 values)
        count (count values)]
    (/ sum count)))

; Function to calculate statistics
(defn generate-statistics [transformed-data]
  (let [grouped-data (group-by #(vector (:relation %) (:departure %) (:destination %)) transformed-data)
        compute-stats (fn [[k v]]
                        (let [paid-values (map :paid v)
                              sorted-values (sort paid-values)
                              median-val (if (odd? (count sorted-values))
                                           (nth sorted-values (quot (count sorted-values) 2))
                                           (compute-mean [(nth sorted-values (quot (count sorted-values) 2))
                                                          (nth sorted-values (dec (quot (count sorted-values) 2)))]))
                              max-val (apply max paid-values)
                              min-val (apply min paid-values)
                              mean-val (compute-mean paid-values)
                              count-val (count paid-values)]
                          {:group-type (nth k 0)
                           :departure (nth k 1)
                           :destination (nth k 2)
                           :max max-val
                           :min min-val
                           :mean mean-val
                           :median median-val
                           :count count-val}))]
    (map compute-stats grouped-data)))

; Analysis function
(defn analyse-data [filename current-year price-increase]
  (let [raw-data (process-csv-data filename)
        transformed-data (transform-data raw-data current-year)
        statistics (generate-statistics transformed-data)
        success-rates (predict-future-sales transformed-data)
        future-sales-predictions (predict-future-sales transformed-data)
        purchase-probabilities (calculate-purchase-probabilities transformed-data price-increase statistics success-rates future-sales-predictions)]
    (println "Simple Statistics:")
    (doseq [stat statistics]
      (println stat))
    (println "\nRate of Success:")
    (doseq [rate success-rates]
      (println rate))
    (println "\nPredicted Future Sales:")
    (doseq [prediction future-sales-predictions]
      (println prediction))
    (println "\nPurchase Probabilities:")
    (doseq [probability purchase-probabilities]
      (println probability))))

; Analysis function call
(analyse-data "/Users/kenny/clojure-projects/src/sales_team_5.csv" 2023 4)

