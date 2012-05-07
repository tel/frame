(defproject framework/frame "0.0.1-SNAPSHOT"
  :description "Layout engine for SVG in Hiccup; the nuts and bolts of Clojure SVG graphics."
  :url "https://github.com/tel/frame"
  :license {:name "MIT License"
            :url "http://www.opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [slingshot "0.10.2"]
                 [hiccup "1.0.0"]]
  :profiles {:dev {:warn-on-reflection true
                   :dependencies [[expectations "1.3.7"]]}})