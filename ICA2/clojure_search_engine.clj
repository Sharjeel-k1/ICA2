(ns closure-projects.ICA2.clojure_search_engine
  (:gen-class)
  (:require
    [clojure.string :as str]))


; Define graph structure
(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (atom {}) (atom {})))

;; Defining vertex structure
(defrecord Vertex [label neighbors])
(defn make-vertex [label]
  (Vertex. label (atom '())))

(defn graph-add-vertex! [graph label]
  (let [vertices (:vertices graph)
        new-vertex (make-vertex label)]
    (swap! vertices assoc label new-vertex))
  nil)

;; Edge structure definition
(defrecord Edge [from to label weight])
(defn make-edge [from to label weight]
  (Edge. from to label weight))

(defn graph-edge-key [from to]
  (sort (list from to)))

;; Add a new edge to the graph's edges atom and update neighbors for both vertices
(defn graph-add-edge! [graph from to label weight]
  (let [vertices (:vertices graph)
        from-vertex (get @vertices from)
        to-vertex (get @vertices to)
        from-vertex-neighbors @(:neighbors from-vertex)
        to-vertex-neighbors @(:neighbors to-vertex)
        new-edge (make-edge from to label weight)
        new-edge-key (graph-edge-key from to)]
    (swap! (:edges graph) assoc new-edge-key new-edge)
    (reset! (:neighbors from-vertex) (conj from-vertex-neighbors to))
    (reset! (:neighbors to-vertex) (conj to-vertex-neighbors from))))

;; Implemented functions to handle the graph
(defn graph-has-vertex? [graph label]
  (contains? @(:vertices graph) label))

(defn get-edge-weight [graph from to]
  (:weight (get @(:edges graph) (graph-edge-key from to))))

;; Define the graph
(def g (make-graph))

;; Inserting vertices and edges manually
(doseq [data [["Krakov" "Warsaw" "100"]
              ["Hamburg" "Berlin" "100"]
              ["Warsaw" "Berlin" "300"]
              ["Prague" "Berlin" "200"]
              ["Munich" "Berlin" "100"]
              ["Munich" "Innsbruck" "100"]
              ["Vienna" "Innsbruck" "200"]
              ["Vienna" "Budapest" "300"]
              ["Warsaw" "Budapest" "400"]
              ["Zagreb" "Budapest" "200"]
              ["Vienna" "Rome" "400"]
              ["Napoli" "Rome" "200"]
              ["Napoli" "Rijeka" "100"]
              ["Vienna" "Prague" "200"]
              ["Vienna" "Rijeka" "400"]
              ["Rijeka" "Zagreb" "100"]
              ["Vienna" "Zagreb" "300"]
              ["Munich" "Zagreb" "400"]
              ["Innsbruck" "Rome" "400"]
              ["Budapest" "Rome" "400"]
              ["Budapest" "Berlin" "300"]
              ["Prague" "Brno" "100"]
              ["Prague" "Budapest" "300"]]]
  (loop [data data]
    (when (seq data)
      (let [vec (first data)
            cities (take 2 vec)
            weight-str (get vec 2)]
        (doseq [city cities]
          (if (not (graph-has-vertex? g city))
            (graph-add-vertex! g city)))
        (let [from (get vec 0)
              to (get vec 1)
              weight (try
                       (Integer/parseInt weight-str)
                       (catch Exception _ 0))]
          (try
            (graph-add-edge! g from to (str from " " to " " weight) weight)
            (catch Exception e
              (println (str "Error adding edge: " vec ", Message: " (.getMessage e)))))
          (recur (rest data)))))))

;; Uncomment to see the edges and vertices of the graph
;(doseq [vertex @(:vertices g)]
;  (println vertex))
;

;doseq [edge @(:edges g)]
  ;(println edge)
;)

;; Get the neighbors of a vertex with the given label in the graph
(defn graph-get-neighbors [graph label]
  (let [vertex (get @(:vertices graph) label)]
    (if vertex
      @(:neighbors vertex)
      (do (println (str "No vertex found for label " label))
          []))))

;; Check if the given path satisfies cost, budget, and max-flights constraints
(defn check-constraints [cost budget path max-flights]
  (if (and (<= cost budget)
           (< (- (count path) 1) max-flights))
    true
    false))

;; Use breadth-first search to find travel plans satisfying constraints
(defn bfs-find-plans [graph start-label end-city-spec budget max-flights]
  (let [start-cost (get-edge-weight graph start-label start-label)
        queue (ref [[{:vertex start-label :cost (or start-cost 0)}]])
        plans (atom [])]
    (while (not (empty? @queue))
      (let [path (first @queue)]
        (dosync
          (ref-set queue (rest @queue)))
        (let [current-vertex (-> path last :vertex)
              current-cost (-> path last :cost)]
          (when (and (and (string? end-city-spec) (= current-vertex end-city-spec))
                     (check-constraints current-cost budget path max-flights))
            (swap! plans conj {:path (map (fn [p] {:city (:vertex p) :cost (:cost p)}) path) :total-cost current-cost}))
          (when (not (= current-vertex end-city-spec))
            (let [neighbors (graph-get-neighbors graph current-vertex)]
              (doseq [neighbor neighbors]
                (let [edge-cost (get-edge-weight graph current-vertex neighbor)
                      total-cost (+ current-cost edge-cost)]
                  (when (and (not (some #(= neighbor (:vertex %)) path))
                             (check-constraints total-cost budget path max-flights))
                    (dosync
                      (alter queue conj (conj path {:vertex neighbor :cost total-cost})))))))))))
    @plans))

;; Sort travel plans by total cost and number of flights
(defn sort-plans [plans]
  (sort-by (juxt (comp - :total-cost) (comp count :path)) plans))

;; Remove duplicate paths based on the number of flights
(defn remove-duplicate-paths [plans]
  (let [seen-flights (atom #{})]
    (filter (fn [plan]
              (let [num-flights (- (count (:path plan)) 1)]
                (if (contains? @seen-flights num-flights)
                  false
                  (do
                    (swap! seen-flights conj num-flights)
                    true))))
            plans)))

;; Find and sort travel plans based on constraints
(defn find-and-sort-plans [graph start-label end-city-name budget max-flights]
  (let [raw-plans (bfs-find-plans graph start-label end-city-name budget max-flights)]
    (let [filtered-plans (filter
                           (fn [plan]
                             (and (<= (:total-cost plan) budget)
                                  (< (- (count (:path plan)) 1) max-flights)))
                           raw-plans)]
      (let [sorted-plans (sort-plans filtered-plans)
            distinct-plans (remove-duplicate-paths sorted-plans)
            most-expensive-plan (first distinct-plans)
            cheapest-plan (last distinct-plans)]
        (if (= most-expensive-plan cheapest-plan)
          [most-expensive-plan]
          [most-expensive-plan cheapest-plan])))))

;; Classify people based on the given conditions
(defn people-classification [people]
  (let [surnames (atom #{})
        ysob (atom #{})]
    (doseq [person people]
      (let [[surname yob] (clojure.string/split (first person) #" ")]
        (swap! surnames conj surname)
        (swap! ysob conj yob)))
    (and (= 1 (count @surnames))
         (some #(> (clojure.edn/read-string %) 2006) @ysob)
         (some #(< (clojure.edn/read-string %) 2006) @ysob))))

;; Get the predicted budget based on historical data and customer type
(defn get-predicted-budget [historical-file customer-type departure-city destination-city & more]
  (let [print-lines? (if (empty? more) false (first more))
        historical-data (clojure_analysis/process-csv-data historical-file)
        statistics (clojure_analysis/generate-statistics (clojure_analysis/transform-data historical-data 2024))
        direct-route (filter #(and (= (:departure %) departure-city)
                                   (= (:destination %) destination-city))
                             statistics)
        reverse-route (filter #(and (= (:departure %) destination-city)
                                    (= (:destination %) departure-city))
                              statistics)
        filtered-stats (if (empty? direct-route) reverse-route direct-route)
        budget-output (atom 1000)
        customer-type-transformed (if customer-type "family" "group")
        stats-for-type-general (try
                                 (clojure_analysis/compute-mean
                                   (map :mean
                                        (filter #(= (:group-type %) customer-type-transformed) statistics)))
                                 (catch Exception e 0))]

    (if (or (empty? filtered-stats) (empty? (filter #(= (:group-type %) customer-type-transformed) filtered-stats)))
      (do
        (if (= 0 stats-for-type-general)
          (reset! budget-output (clojure_analysis/compute-mean  (map :mean statistics)))
          (reset! budget-output stats-for-type-general)))

      (reset! budget-output (:max (first (filter #(= (:group-type %) customer-type-transformed) filtered-stats)))))

    (if print-lines?
      (println "Estimated Predicted Budget Is: " @budget-output))

    @budget-output))

;; Format the route data for better display
(defn format-route [data]
  (->> data
       (map #(str (:city %) " (" (:cost %) ")"))
       (clojure.string/join " -> ")))

;; Get the cheapest ticket price from the plans
(defn get-cheapest-ticket-price [plans]
  (let [plan (last plans)]
    (let [{:keys [path total-cost]} plan]
      (println "For Path: " (format-route path))
      total-cost)))

(def total-profit (atom 0))

;; Prepare the travel plan based on given inputs
(defn prepare-travel-plan [departure-city destination-city people & more]
  (let [print-lines? (if (empty? more) false (first more))]
    (when (not (empty? @(:vertices g)))
      (let [budget (get-predicted-budget "/Users/kenny/clojure-projects/src/sales_team_5.csv"
                                         (people-classification people)
                                         departure-city
                                         destination-city
                                         print-lines?)
            rounded-budget (* 100 (Math/floor (/ budget 100)))
            max-cities (if (people-classification people) 4 5)
            plans (find-and-sort-plans g departure-city destination-city budget max-cities)
            ticket-price (get-cheapest-ticket-price plans)]

        (cond
          (nil? (first plans)) (do
                                 (if print-lines?
                                   (println "Plan Not Found" departure-city "to" destination-city))
                                 ##Inf)

          (< rounded-budget ticket-price) (do
                                            (if print-lines?
                                              (println "Low Budget, Ticket Cannot Be Purchased"))
                                            ##Inf)

          :else (do
                  (reset! total-profit (+ @total-profit (* (- rounded-budget ticket-price) (count people))))
                  (if print-lines?
                    (do
                      (println "Price of Ticket is: " ticket-price)
                      (println "Sold to Customer: " rounded-budget)
                      (println "Ticket Profit: " (- rounded-budget ticket-price))
                      (println "Total Profit is" @total-profit)))
                  rounded-budget))))))

;;example call to generate a travel plan based on input parameters
(prepare-travel-plan  "Berlin"
                     "Napoli"
                     [["Harry Adams", 1991]
                      ["Elsie Adams", 1976]
                      ["Alfie Adams", 2017]
                      ["Elsie Adams", 2016]]
                     true)


