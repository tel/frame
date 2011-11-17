(defproject frameworke/frame "0.0.1-SNAPSHOT"
  :description "Nuts and bolts graphical object writing."
  :warn-on-reflection true
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/algo.monads "0.1.0"]
                 [slingshot "0.8.0"]
                 [hiccup "0.3.7"]]
  :target-dir "target/"
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :clj-stacktrace {:test-color true})