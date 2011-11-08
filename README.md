# frame

A thin wrapper over Hiccup for low-level plotting and layout.

Wrap a Hiccup data structure in a frame to use context aware flows,
data aware painting and natural scales. It translates transparently to
Hiccup-flavored SVG and Hiccup syntax can be used liberally.

## rationale

I do a lot of scientific work in Clojure, but the storytelling aspects
always end up living in R due to the fantastic plotting libraries
(grid, lattice, ggplot2) available. The export/import/plot workflow is
clunky and I'd also like to incorporate the declarative methods
pioneered by Michael Bostock in protovis/d3.

# The frameworke projects

Frameworke is an SVG-hosted plotting library at three levels: 

1. frame: low-level SVG chart layout and building
2. worke: grammar-of-graphics and data aware composition of middle-level graphing objects
3. plots: very high level default plots for data exploration and discovery using formulae

I'm proudly stealing great ideas from d3 for frame, ggplot2 for worke,
and lattice for plots. Atop it all, Clojure's natural depiction of
tree-like data and homoiconicity creates an elegant DSL for creating
and describing empirical stories in charts.

## Usage

Not implemented yet, but here's the idea

````clojure
;; Paired dotplots sharing an axis
(svg (picture {:width 500 :height 400}
       (hflow [50 [:auto 0.5 0.5]]
         (let [ ;; Scales are context-aware and plot to the proper
               ;; place in each viewport
               ty (scale:linear :auto (concat (map :x data1)
                                              (map :x data2)))
               ;; Scales should ALWAYS be used
               tx1 (scale:linear :auto (map :y data1))
               tx2 (scale:linear :auto (map :y data2))]
           (vaxis :on-scale ty)
           (vflow [1 :auto]
             (padding [0 0 5 0]
               (viewport {:xlim [(tx -2) (tx 30)]}
                 (for [{:keys [x y]} data1]
                   [:circle {:r 1 :cx (tx1 x) :cy (ty y)}])))
             (haxis :on-scale tx1))
           (vflow [1 :auto]
             (padding [5 0 0 0]
               (viewport {}
                 (for [{:keys [x y]} data2]
                   [:circle {:r 1 :cx (tx2 x) :cy (ty y)}])))
             (haxis :on-scale tx2))))))


;; A simple lattice-like dotplot
(svg (picture {:width 500 :height 400}
       (let [tx (scale:log :auto (concat (map :p1 data)
                                         (map :p2 data)
                                         (map :p3 data)))]
         (vflow (repeat (count data) 1)
           (for [{:keys [lab p1 p2 p3]} data]
             (hflow [50 [:auto 1]]
               [:text lab]
               ;; The viewport is set to a 1-dimensional mode; y is centered
               (viewport {:y :center}
                 [:circle {:r 1 :cx (tx p1)}]
                 [:circle {:r 1 :cx (tx p2)}]
                 [:circle {:r 1 :cx (tx p3)}])))
           (hflow [50 [:auto 1]]
             nil
             (haxis :on-scale tx))))))
````

## MIT License

Copyright (C) 2011 Joseph Abrahamson

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

