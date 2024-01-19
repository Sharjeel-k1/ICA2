(ns closure-projects.sortingHat-Sharjeel-khan)
(def houses [:gryffindor :hufflepuff :ravenclaw :slytherin])

(defn sorting-hat []
  (rand-nth houses))

(defn main []
  (println "Welcome to the Hogwarts Sorting Hat!")

  (print "Enter your name: ")
  (flush)
  (def student-name (read-line))

  (println (str "Dear " student-name ",")
           (str "After much consideration, the Sorting Hat has chosen you for")
           (str (sorting-hat)) " house!"))

(main)