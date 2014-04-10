(defproject inliner "0.1.0"
  :description "A simple CSS inliner"
  :url "https://github.com/jamesnvc/inliner"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [enlive "1.1.5"]
                 [factual/fnparse "2.3.0"]]
  :profiles
  {:dev {:plugins [[quickie "0.2.6"]]}})
