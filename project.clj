(defproject myapp "0.0.1"
  :description "React & ClojureScript testing"
  :license {:name "MIT" :url  "https://opensource.org/licenses/MIT"}
  :signing {:gpg-key "9DD8C3E9"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :source-paths ["src"]
  :resource-paths ["resources"]
  :profiles {:cljs {:dependencies [[org.clojure/clojurescript "1.10.520"]]}}
  :aliases {})
