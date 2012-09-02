(ns bieberscore.core
  (:require [clj-http.client :as client]
            [clojure.xml :as xml]
            [clojure.zip :as zip]
            [net.cgrand.enlive-html :as html]
            [stemmers.core]
            )
  (:use clojure.pprint)
  (:use clojure.data.zip.xml)
  (:use [clojure.java.io :only [reader]])
  ;(:use alex-and-georges.debug-repl)
)

(import java.net.URLEncoder)

(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def API-KEY "935ae9bb866dee493ee4bb2e28faecf2")
(def BASE-URL "http://ws.audioscrobbler.com/2.0/")

(defn read-lines [fname]
  (with-open [r (reader fname)]
    (doall  (line-seq r))))

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
  (println "getting lyrics for" artist title)
  (let [url (dbg (gen-wikia-url artist title))]

    (try
      (let [data (html/html-resource (java.net.URL. url))
        ; Im sure this can be done more concisely, get the lyricbox div, remove the rtMatcher div
        ; and select the text nodes from whats left
            lines (html/select (html/at (html/select data [:div.lyricbox]) [:div.rtMatcher] nil) [html/text-node])
            clean-lines (for [line lines] (clojure.string/trim (clojure.string/replace (clojure.string/lower-case line) #"\p{Punct}" "")))]
        (filter #(> (count %) 1) clean-lines)) 
      (catch java.io.FileNotFoundException e '()))))


(defn tracks-for-user [user]
   (map album-tracks (flatten (map top-albums (top-artists "dgorissen")))))

(defn tracks-for-artist [artist]
   (map album-tracks (top-albums artist)))


(defn build-syllable-map
  []
  (let [
        lines (filter #(re-matches #"^[A-Z].*" %) (read-lines "src/bieberscore/cmudict.0.7a.txt"))
        word-counts (for [line lines
                          :let [parts (clojure.string/split line #"\s+" 2)
                                 word (clojure.string/lower-case (first parts))
                                 sylcount (count (re-seq #"\d" (second parts)))]]
                       [word sylcount])]
  
   (println "Loaded syllable map with " (count word-counts) " words")
   (reduce conj {} word-counts)))

(def syllable-map (build-syllable-map))

(defn count-syllables [word]
  (get syllable-map word 0)) 

(defn calc-score [lyrics]
  (if (empty? lyrics)
    0

  (let [words (flatten (for [line (dbg lyrics)] (clojure.string/split line #"\s+")))
        num-sentences (count lyrics)
        num-unique-sentences (count (set lyrics))
        num-words (count words)
        num-unique-words (count (set words))
        num-syls (reduce + (map count-syllables words))
        lexical-density (/ num-unique-words num-words)
        sentence-density (/ num-unique-sentences num-sentences)
        flesch-score (+ 206.825 (* -1.015 (/ num-words num-sentences)) (* -84.6 (/ num-syls num-words)))
        
        ]
    (+ (- 1 lexical-density) (- 1 sentence-density) (/ flesch-score 100)))))
    

(defn -main
  "I don't do a whole lot."
  [& args]
   (let [lyrics 
          (for [albums (tracks-for-artist "Eminem")
            tracks albums
            :let [artist (first tracks)
                  title (second tracks)]]
        

            (scrape-lyrics artist title))
         song (first lyrics)
         ]

     (pprint song)
     (println "Score is " (reduce + (map calc-score lyrics)))
 )) 
