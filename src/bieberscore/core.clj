"Analyzing lyrical complexity, a toy example for learning clojure"
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
)

(import java.net.URLEncoder)

;debugging macro from http://stackoverflow.com/questions/2352020/debugging-in-clojure
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; change to your api key
(def API-KEY "b25b959554ed76058ac220b7b2e0a026")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-lines
  "read the given file into a sequence, one line per element"
  [fname]
  (with-open [r (reader fname)]
    (doall  (line-seq r))))


(defn string-to-xml
  "parse the string into an xml zipper structure"
  [string]
  (zip/xml-zip
   (xml/parse
    (java.io.ByteArrayInputStream.
      (.getBytes (.trim string))))))


(defn get-lastfm-url 
  "Invoke a method on the LastFm API, takes a map representing the URL query parameters and an api key"
  [params]
  (let [api-key API-KEY
        query-params (assoc params "api_key" api-key)                     ; add the api key to the query params
        ps (assoc {} :query-params query-params :throw-exceptions false)  ; dont throw exceptions
        response (client/get "http://ws.audioscrobbler.com/2.0/" ps)]     ; the raw response

    (if (= (response :status) 200)
      (string-to-xml (response :body))
      (do 
        (println "* Problem using lastfm api, parameters")
        (pprint ps)
        (println "* Response:")
        (pprint (response :body))
        nil))))


(defn top-artists
  "Get the top artists for a given lastfm user"
  [user]
  (let [period "3month"
        data (get-lastfm-url {"method" "user.gettopartists" "user" user "limit" "5" "period" period})]
    (if (nil? data)
      []
      (xml-> data :topartists :artist :name text))))


(defn top-albums
  "Get the top albums for a given artist"
  [artist]
  (let [data (get-lastfm-url {"method" "artist.gettopalbums" "artist" artist "limit" "4"})]
    (if (nil? data)
      []
      (xml-> data :topalbums :album :mbid text))))


(defn album-tracks
  "Get all the tracks on a particular album, denoted by the musicbrainz id"
  [mbid]
  (let [data (get-lastfm-url {"method" "album.getinfo" "mbid" mbid})]
    (if (nil? data)
      []
      (let [artist (first (xml-> data :album :artist text)) ]
        (map vector (repeat artist) (xml-> data :album :tracks :track :name text))))))


(defn tracks-for-user 
  "Get the tracks from the top albums of a lastfm user"
  [user]
  (map album-tracks (flatten (map top-albums (top-artists user)))))


(defn tracks-for-artist
  "Get the tracks from the top albums of an artist"
  [artist]
  (map album-tracks (top-albums artist)))


(defn gen-wikia-url
  "Generate a wikia url for the given track"
  [artist title]
  (str "http://lyrics.wikia.com/"
    (URLEncoder/encode (clojure.string/replace artist " " "_"))
     ":"
    (URLEncoder/encode (clojure.string/replace title " " "_"))))


(defn scrape-lyrics
  "Scrape the lyrics off wikia for the given track"
  [artist title]
  (let [url (gen-wikia-url artist title)]

    (try
      (let [data (html/html-resource (java.net.URL. url))
            ; Im sure this can be done more concisely, get the lyricbox div, remove the rtMatcher div
            ; and select the text nodes from whats left
            lines (html/select (html/at (html/select data [:div.lyricbox]) [:div.rtMatcher] nil) [html/text-node])
            ; lowercase all words, strip whitespace, and remove punctuation
            clean-lines (for [line lines] (clojure.string/trim (clojure.string/replace (clojure.string/lower-case line) #"\p{Punct}" "")))]

        (println "* Got lyrics for" artist title)

        (filter #(> (count %) 1) clean-lines)) ; ignore empty lines
      (catch java.io.FileNotFoundException e (println "* Failed to get lyrics for" artist title)))))


(defn build-syllable-map
  "Build up a hashmap that maps a word to the number of syllables it has, based on the CMU Pronouncing Dictionary"
  []
  (let [
        ; only keep the lines we actually care about
        lines (filter #(re-matches #"^[A-Z].*" %) (read-lines "src/bieberscore/cmudict.0.7a.txt"))
        ; create a list of [word num-syllables] pairs using list comprehension
        word-counts (for [line lines
                          :let [parts (clojure.string/split line #"\s+" 2)        ; split into word and pronounciation parts
                                 word (clojure.string/lower-case (first parts))   ; the first part is the word
                                 sylcount (count (re-seq #"\d" (second parts)))]] ; from the second part extract all the digits
                                                                                  ; the number of digits indicates the number of syllables
                       [word sylcount])]

   (println "Loaded syllable map with" (count word-counts) "words")
   (reduce conj {} word-counts))) ; turn the sequence of pairs into a map


"Define the global syllable map, what's the proper way for doing this in clojure?"
(def syllable-map (build-syllable-map))


(defn approx-syllables
  "A very (!) simple approximation for counting syllables"
  [word]
  (let [w (stemmers.porter/stem word)           ; first reduce to the stem
       vsplits (count (re-seq #"[aeiouy]+" w))] ; split on vowels

       (if (= (last word) "e")
         (- vsplits 1)
         vsplits)))


(defn count-syllables
  "How many syllables in the given word"
  [word]
  (if (< (count word) 3) ; simple speed optimization, words with less than 3 letters only have 1 syllable
    1
    (get syllable-map word (approx-syllables word))))


(defn calc-score
  "Calculate the score for the given lyrics, the higher the score the, more bieberesque the lyrics:
  
  score = (1 - (proporion of unique words)) + (1 - proportion of unique sentences) + (Flesch-Kincaid readibility score)/100

  This is just arbitrary really and shouldn't be taken seriously as there are lots of issues (e.g., non-english lyrics)
  Think of it more as spielerei.
  "
  [lyrics]
  (if (empty? lyrics)
    0
    (let [words (flatten (for [line lyrics] (clojure.string/split line #"\s+")))  ; split into a sequence of words
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


(defn bieberscore 
  "Top level function, calculate the bieberscore using the given function as a source of tracks."
  [track-source]
  (let [lyrics
          (for [albums (track-source)
                tracks albums
                :let [artist (first tracks)
                      title (second tracks)]]

            (scrape-lyrics artist title))

         num-tracks (count lyrics)]

     (if (< num-tracks 1)
       "Sorry, artist unknown"
       ; calculate the score for every song and average
       (/ (reduce + (map calc-score lyrics)) num-tracks))))


(defn -main
  "Main function."
  [& args]
  (println "")

  ; to get the bieberscore for a particular artist
  (println "The bieberscore is:" (bieberscore (partial tracks-for-artist "rihanna")))


  ; to get the bieberscore for a particular lastfm user
  ;(println "The bieberscore is:" (bieberscore (partial tracks-for-user "dgorissen")))

)
