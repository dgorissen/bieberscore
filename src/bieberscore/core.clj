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

(import java.net.URLEncoder)

(def API-KEY "935ae9bb866dee493ee4bb2e28faecf2")
(def BASE-URL "http://ws.audioscrobbler.com/2.0/")

(defn string-to-xml [string]
  (zip/xml-zip
   (xml/parse
    (java.io.ByteArrayInputStream.
      (.getBytes (.trim string))))))


(defn get-lastfm-url [url params]
  (let [response (client/get url (assoc params :throw-exceptions false))]
    (if (= (response :status) 200)
      (string-to-xml (response :body))
      nil)))

(defn top-artists [user]
  (let [
        period "3month"
        data (get-lastfm-url BASE-URL {:query-params {"method" "user.gettopartists"
                                                        "user" user
                                                        "limit" "5"
                                                        "api_key" API-KEY
                                                        "period" period}})
        ]
    (if (nil? data)
      []
      (xml-> data :topartists :artist :name text))))


(defn top-albums [artist]
  (let [
        data (get-lastfm-url BASE-URL {:query-params {"method" "artist.gettopalbums"
                                                   "artist" artist
                                                   "limit" "3"
                                                   "api_key" API-KEY}})
        ]
    (if (nil? data)
      []
      (xml-> data :topalbums :album :mbid text))))

(defn album-tracks [mbid]
  (let [
        data (get-lastfm-url BASE-URL {:query-params {"method" "album.getinfo"
                                                      "mbid" mbid
                                                       "api_key" API-KEY}})
        ]
    (if (nil? data)
      []
      (let [artist (first (xml-> data :album :artist text)) ]
        (map vector (repeat artist) (xml-> data :album :tracks :track :name text))))))

(defn gen-wikia-url [artist title]
  (str "http://lyrics.wikia.com/"
    (URLEncoder/encode (clojure.string/replace artist " " "_"))
     ":"
    (URLEncoder/encode (clojure.string/replace title " " "_"))))

(defn scrape-lyrics [artist title]
  (let [url (gen-wikia-url artist title)]

    (try
      (let [data (html/html-resource (java.net.URL. url))]
        ; Im sure this can be done more concisely, get the lyricbox div, remove the rtMatcher div
        ; and select the text nodes from whats left
        (filter
          #(> (count %1) 1) 
          (html/select (html/at (html/select data [:div.lyricbox]) [:div.rtMatcher] nil) [html/text-node] )))
      (catch java.io.FileNotFoundException e '()))))

(defn collect-tracks [user]
   (map album-tracks (flatten (map top-albums (top-artists "dgorissen")))))


(defn -main
  "I don't do a whole lot."
  [& args]
   (let [lyrics 
          (for [albums (collect-tracks "dgorissen")
            tracks albums
            :let [artist (first tracks)
                  title (second tracks)]]
        

            (scrape-lyrics artist title))]

     (pprint (nth lyrics 4))            
     )
 ) 
