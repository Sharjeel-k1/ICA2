(ns closure-projects.prac)
(def flights-data
  [{:from "Krakov" :to "Warsaw" :price 100}
   {:from "Hamburg" :to "Berlin" :price 100}
   {:from "Warsaw" :to "Berlin" :price 300}
   {:from "Prague" :to "Berlin" :price 200}
   {:from "Munich" :to "Berlin" :price 100}
   {:from "Munich" :to "Innsbruck" :price 100}
   {:from "Vienna" :to "Innsbruck" :price 200}
   {:from "Vienna" :to "Budapest" :price 300}
   {:from "Warsaw" :to "Budapest" :price 400}
   {:from "Zagreb" :to "Budapest" :price 200}
   {:from "Vienna" :to "Rome" :price 400}
   {:from "Napoli" :to "Rome" :price 200}
   {:from "Napoli" :to "Rijeka" :price 100}
   {:from "Vienna" :to "Prague" :price 200}
   {:from "Vienna" :to "Rijeka" :price 400}
   {:from "Rijeka" :to "Zagreb" :price 100}
   {:from "Vienna" :to "Zagreb" :price 300}
   {:from "Munich" :to "Zagreb" :price 400}
   {:from "Innsbruck" :to "Rome" :price 400}
   {:from "Budapest" :to "Rome" :price 400}
   {:from "Budapest" :to "Berlin" :price 300}
   {:from "Prague" :to "Brno" :price 100}
   {:from "Prague" :to "Budapest" :price 300}])


(defn find-flights [client-type from-city to-city]
  (let [max-connections (if (= client-type :family) 2 3)
        max-price (if (= client-type :family) 700 1000)]
    (->> flights-data
         (filter #(and (= (:from %) from-city)
                       (= (:to %) to-city)))
         (filter #(<= (:price %) max-price))
         (filter #(<= (count (conj (set [(:from %) (:to %)]) (:to %))) max-connections))
         (sort-by :price))))

(defn main []
  (println "Welcome to the flight booking system!")
  (println "Please enter your client type (family/tour): ")
  (let [client-type (read-line)]
    (println "Please enter the departure city: ")
    (let [from-city (read-line)]
      (println "Please enter the destination city: ")
      (let [to-city (read-line)]
        (println "Recommended flights:")
        (doseq [flight (find-flights client-type from-city to-city)]
          (println (str "From " from-city " to " to-city ", Price: $" (:price flight))))))))

(main)