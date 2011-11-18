# frame

A thin wrapper over Hiccup for low-level plotting and layout.

Wrap a Hiccup data structure in Frames with context aware flowing,
data aware painting, and natural scales. It translates (semi-)transparently 
to Hiccup-flavored SVG allowing obvious mixing between the two libraries.

*Note*: As a `ver < 1` library, the API is considered super flexible.

# The frameworke projects

Frameworke is a declarative, SVG-hosted plotting library at three levels: 

1. frame: low-level SVG chart layout and building
2. worke: data aware composition of middle-level graphing objects
3. plots: high level ploting for data exploration and discovery using formulae

Framework happily steals ideas from d3, Trellis, and GGPlot, bringing 
sensible plotting to Clojure.

## Usage

````clojure
;; Paired dotplots sharing an axis
(spit "out.svg"
      (test-svg
       (doframes {:width 500 :height 400}
         (dataframe [ty (scale/linear :domain (concat (map :x data1)
                                                      (map :x data2))
                                      :range :y)
                     tr (scale/sqrt :domain (concat (map :r data1)
                                                    (map :r data2))
                                    :range [1 6])]
           (hflow
            (padded [0 0 5 0]
              (dataframe
                  [tx (scale:linear :domain (map :x data1) :range :x)]
                (for [{:keys [x y r]} data1]
                  [:circle {:fill "#000" :r (tr 1) :cx (tx x) :cy (ty y)}])))
            (padded [5 0 0 0]
              (dataframe
                  [tx (scale:linear :domain (map :x data1) :range :x)]
                (for [{:keys [x y r]} data2]
                  [:circle {:fill "#000" :r (tr 1) :cx (tx x) :cy (ty y)}]))))))))
````

## Other projects and inspiration
* Mike Bostock's [d3](http://mbostock.github.com/d3/)
* R's [Trellis Graphics](http://www.stat.auckland.ac.nz/~paul/RGraphics/chapter4.pdf) library
* Hadley Wickham's [GGPlot2](http://www.stat.auckland.ac.nz/~paul/RGraphics/chapter4.pdf)
* David Edgar Liebke's [Annalemma](http://liebke.github.com/analemma/)

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

