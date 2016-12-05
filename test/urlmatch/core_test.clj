(ns urlmatch.core-test
  (:require [clojure.test :refer :all]
            [urlmatch.core :refer :all])
  (:import [urlmatch.core Pattern])
  )

(deftest twitter-recognize
  (let [
         twitter (Pattern. "host(twitter.com); path(?user/status/?id);")
         url-right (recognize twitter "http://twitter.com/bradfitz/status/562360748727611392")
         url-err (recognize twitter "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
         ]
    (is (= url-right [[:id "562360748727611392"] [:user "bradfitz"]]))
    (is (nil? url-err))
    )
  )

(deftest dribble-recognize
  (let [
         dribble   (Pattern. "host(dribbble.com); path(shots/?id); queryparam(offset=?offset);")
         url-right (recognize dribble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
         url-err   (recognize dribble "https://twitter.com/shots/1905065-Travel-Icons-pack?list=users&offset=1")
         query-param-miss (recognize dribble "https://dribbble.com/shots/1905065-Travel-Icons-pack?list=users")
        ]
    (is (= url-right [[:id "1905065-Travel-Icons-pack"] [:offset "1"]]))
    (is (nil? url-err))
    (is (nil? query-param-miss))
    )
  )
