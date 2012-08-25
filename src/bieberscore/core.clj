(ns bieberscore.core
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [net.cgrand.enlive-html :as html]
            )
  (:use clojure.pprint)
  (:use clojure.data.zip.xml)
  ;(:use alex-and-georges.debug-repl)

)

(def API-KEY "935ae9bb866dee493ee4bb2e28faecf2")
(def BASE-URL "http://ws.audioscrobbler.com/2.0/")

(defn string-to-xml [string]
  (zip/xml-zip
   (xml/parse
    (java.io.ByteArrayInputStream.
      (.getBytes (.trim string))))))

(defn top-artists [user]
  (let [
        period "3month"
        data ((client/get BASE-URL {:query-params {"method" "user.gettopartists" 
                                                   "user" user 
                                                   "limit" "50"
                                                   "api_key" API-KEY 
                                                   "period" period}}) :body)
        xmls (string-to-xml data)
        ]
    (xml-> xmls :topartists :artist :name text)
   )
)

(defn top-albums [artist]
  (let [
        data ((client/get BASE-URL {:query-params {"method" "artist.gettopalbums"
                                                   "artist" artist
                                                   "limit" "50"
                                                   "api_key" API-KEY}}) :body)
        xmls (string-to-xml data)
        ]
    (xml-> xmls :topalbums :album :mbid text)
   )
)

(defn album-tracks [mbid]
  (let [
        data ((client/get BASE-URL {:query-params {"method" "album.getinfo"
                                                   "mbid" mbid
                                                   "api_key" API-KEY}}) :body)
        xmls (string-to-xml data)
        ]
    (xml-> xmls :album :tracks :track :name text)
   )
)

(defn scrape-lyrics [artist title]
  (let [url (str "http://lyrics.wikia.com/" artist ":" title)
        data (html/html-resource (java.net.URL. url))
        ]
    (map html/text (html/at (html/select data [:div.lyricbox]) [:div.rtMatcher] nil))
    )
)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println (album-tracks (first (top-albums (first (top-artists "dgorissen"))))))
  (pprint (count (scrape-lyrics "eminem" "stan")))
  )
