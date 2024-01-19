(ns closure-projects.search)
(def documents
  [{:id 1
    :title "Document 1"
    :content "This is the content of Document 1. Clojure is a great language."
    :tags ["Clojure" "Document"]}

   {:id 2
    :title "Document 2"
    :content "Clojure is a functional programming language."
    :tags ["Clojure" "Programming"]}])
(defn index-documents [documents]
  (reduce
    (fn [index document]
      (reduce
        (fn [idx word]
          (update idx word (fnil conj []) (:id document)))
        index
        (clojure.string/split (:content document) #"\s+")))
    {}
    documents))

(def index (index-documents documents))
(defn search [index query]
  (let [terms (clojure.string/split query #"\s+")
        matching-doc-ids (apply clojure.set/intersection (map index terms))]
    (filter #(contains? matching-doc-ids (:id %)) documents)))
(println (search index "Clojure")) ; Returns documents related to Clojure
(println (search index "Functional Programming")) ; Returns documents related to functional programming
