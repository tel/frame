(defproject frameworke/frame "0.0.1-SNAPSHOT"
  :description "Nuts and bolts graphical object writing."
  :warn-on-reflection true
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/core.match "0.2.0-alpha6"]
                 [hiccup "0.3.7"]]
  :target-dir "target/"
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+])