(ns closure-projects.ICA2.sales-routine
  (:require [clojure_airline-broker :as broker])
  ; TODO replace this link with your engine
  (:require [clojure_search_engine])
  )


; TODO SET YOUR TEAM NUMBER: 1-7
(def team_number 5)
(def search_ticket_function clojure_search_engine)
(broker/run team_number search_ticket_function)
