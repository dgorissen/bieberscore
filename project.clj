(defproject bieberscore "1.0"
  :description "Analyzing lyrical complexity, a toy example for learning clojure"
  :url "https://github.com/dgorissen/bieberscore"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :main bieberscore.core
  :dependencies [
                 [org.clojure/clojure "1.4.0"]
                 [clj-http "0.5.3"]
                 [org.clojure/data.zip "0.1.1"]
                 [enlive "1.0.1"]
                 [stemmers "0.2.1"]
                 ]
  )
