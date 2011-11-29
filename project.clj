(defproject frameworke/frame "0.0.1-SNAPSHOT"
  :description "Nuts and bolts graphical object writing."
  :warn-on-reflection true
  :dev-dependencies [[com.googlecode.efficient-java-matrix-library/ejml "0.17"]
                     [org.clojure/tools.cli "0.1.0"]
                     [die-roboter "1.0.0-SNAPSHOT"]
                     [cljfastdtw "1.0.4-SNAPSHOT"]
                     [congomongo "0.1.7"]]
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/algo.monads "0.1.0"]
                 [slingshot "0.8.0"]
                 [hiccup "0.3.7"]]
  :target-dir "target/"
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :clj-stacktrace {:test-color true})