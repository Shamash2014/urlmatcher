(ns urlmatch.core
  (require [clojure.string :as str]
           [clojure.walk :refer :all]
           ))

(defprotocol IRecogniseable
  (recognize [pattern url])
  )

(defn build-patterns [str-patterns]
  (let [
         with-regexp (fn [pattern-str] (re-seq #"(.*?)\((.*?)\)" pattern-str))

         splitted (->> (str/split str-patterns #";")
                      (map str/trim)
                      (mapcat with-regexp)
                      (map rest))

         all-query (mapcat rest (filter #(= (first %1) "queryparam") splitted))


         result (filter #(not= (first %1) "queryparam") splitted)
       ]
      (-> (apply hash-map (flatten result))
           (keywordize-keys)
           (assoc :queryparams all-query))
    ))

(defn match-query [pattern url]
  (if (boolean (re-find #"\?" pattern))
    (let [
           binds (mapcat rest (re-seq #"\?([A-z]+)\/?=?" pattern))
           escaped-pattern (str/replace pattern #"\?([A-z]+)\/?" "(.+)/?")
         ]

        (map vector
          (flatten (map keyword binds))
          (->> (re-seq (re-pattern escaped-pattern) url)
               (mapcat rest)
               (map (partial re-find #"([\w-]+)"))
               (mapcat rest)
               ))
      )

    (= pattern url)
    )
  )

(defn unite [patterns url]
  (let [
         host-match (= (:host patterns) (:host url))
       ]
   (when (true? host-match)

     (let [
             path-match (match-query (:path patterns) (:path url))
             query-match (apply concat
                                (for [
                                        query (:queryparams patterns)
                                        :let [queried (match-query query (:query url))]
                                     ]
                                   queried))
             result (apply concat (vector path-match query-match))
          ]
       (when (seq? result) (vec (sort-by first result)))
     )))
)

(defrecord Pattern [matching]
  IRecogniseable (recognize [_pattern url]
                            (let [
                                   pattern-map (build-patterns matching)
                                   uri (java.net.URI. url)
                                   host (.getHost uri)
                                   path (.getPath uri)
                                   query (.getQuery uri)
                                 ]
                              (unite pattern-map (zipmap [:host :path :query] [host path query]))))
  )
