(ns frame.core
  (:use hiccup.core)
  (:require [hiccup.page-helpers :as helpers]
            [frame.fstate :as fst]
            [clojure.algo.monads :as m]
            [frame.scale :as scale]
            [clojure.string :as str]))

;;; Some XML and SVG Hiccuphelpers
;;; 
(def doctype (merge helpers/doctype
                    {:svg "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"}))

(def ?xml
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")

(defn svg-tag [attrs body]
  [:svg (merge {:xmlns "http://www.w3.org/2000/svg"
                :xmlns:xlink "http://www.w3.org/1999/xlink"
                :version "1.1"} attrs) body])

(defn svg [body]
  ;; Eventually we'll parse down for sanity in dimensions
  (fst/with-ctx! [width height]
    (svg-tag {:width width :height height :fill :none} body)))

(defn- to-f
  "Convert some Hiccup-like vectors to sequences of f-exprs."
  ([form & forms] (to-f (cons form forms)))
  ([form]
     (cond (sequential? form)
           (let [[head & rest] form]
             (if (not (empty? rest))
               (if (keyword? head)
                 (apply str
                        (subs (str (first form)) 1)
                        "(" (concat (interpose ", " (map to-f
                                                         rest)) [")"]))
                 (apply str (interpose " " (map to-f form))))
               (str head)))
           
           (number? form)
           (str (float form))

           :else
           (str form))))

(defn css
  "Convert a hash to a css style declaration using to-f."
  ([& keys]
     (let [hash (apply hash-map keys)]
       (apply str
              (for [[k v] hash]
                (str (subs (str k) 1) ": "
                     (if (sequential? v) (to-f v) v)
                     "; "))))))

;;; Blocklike elements
;;;

(defn run-frames [ctx frame]
  (first (frame ctx)))

(defn frame* [form]
  (fst/with-ctx [svg?]
    (if svg?
      (fst/bubble-sfn form)
      (svg (fst/set-ctx [svg? true]
             (frame* form))))))

(defmacro frame [& forms]
  `(frame*
    (m/with-monad m/identity-m
      (list ~@forms))))

(defn clipframe* [form]
  (frame
    (fst/with-ctx! [width height]
      (let [clipname (gensym 'clippath)]
        [:g
         [:defs
          [:clipPath {:id clipname} [:rect {:width width :height height}]]]
         [:g {:style (css :clip-path (to-f [:url (str "#" clipname)]))}
          form]]))))

(defmacro clipframe [& forms]
  `(clipframe* (list ~@forms)))

(defmacro dataframe
  "Set up a frame with local scales.
  
   (dataframe
       [tx (scale/affine :domain xs     :range :x)
        ty (scale/affine :domain [-1 1] :range :y)
        tr (scale/sqrt                  :range [1 6])]
    ...)"
  [scale-decls & body]
  (let [pairs (partition 2 scale-decls)
        scales (map second pairs)
        ;; These might not be the best ways to find the x and y scales
        x-scale (first (filter (partial some #(= :x %)) scales))
        y-scale (first (filter (partial some #(= :y %)) scales))]
    `(fst/set-ctx*
         ;; Put the most recent scales into context
         [:x-scale ~x-scale
          :y-scale ~y-scale]
       (m/domonad fst/state-m
         [~@(apply concat
                   (map (fn [[sym scale-form]]
                          `(~sym ~scale-form))
                        (partition 2 scale-decls)))
          res# (frame ~@body)]
         res#))))

(defn- expand-margin
  "Expand the SVG margin syntax."
  ([a] [a a a a])
  ([a b] [a b a b])
  ([a b c d] [a b c d]))

(defn padded
  [padding form]
  (let [[a b c d] (apply expand-margin padding)
        dy (+ a c)
        dx (+ b d)]
    ;; Shrink the inner context
    (fst/update-ctx [width #(- % dx)
                     height #(- % dy)]
      ;; And transform it away from the upper left
      ;; corner
      (frame
        [:g {:transform (to-f :translate d a)}
         form]))))

;;; Flows

(defn- partition-interval
  "Partition an interval using a flexible interval set."
  [int part])

(defn- flatten-seqs
  "Flatten seqs like Hiccup will..."
  [s] (if (seq? s)
        (apply concat
               (map flatten-seqs s))
        (list s)))

(defn hflow
  "Start with even hflow"
  [& things]
  (fst/with-ctx [width]
    (let [things (flatten-seqs things)
          n (count things)
          dx (float (/ width n))]
      (map (fn [thing pos]
             (fst/set-ctx [width dx]
               [:g {:transform (to-f :translate pos 0)} thing]))
           things
           (iterate #(+ dx %) 0)))))

(defn vflow
  "Start with even hflow"
  [& things]
  (fst/with-ctx [height]
    (let [things (flatten-seqs things)
          n (count things)
          dy (float (/ height n))]
      (map (fn [thing pos]
             (fst/set-ctx [height dy]
               [:g {:transform (to-f [:translate 0 pos])} thing]))
           things
           (iterate #(+ dy %) 0)))))

;;; Grobs
(defn path
  ([ds] (path ds :line))
  ([ds key]
     (let [letters {:line "L"
                    :curve "T"}
           [x0 y0] (first ds)]
       (apply str "M" x0 " " y0 " "
              (for [[x y] (rest ds)]
                (str (key letters) x " " y " "))))))

;;; Readers
(defn swallow-exception [f]
  (fn [& args]
    (try (apply f args)
         (catch Exception e nil))))

(defn read-csv [file format]
  (let [letter {\f #(Float/parseFloat %)
                \i #(Integer/parseInt %)
                \s str/trim}
        ;; Is there not a better way to do this? Zip-with-functions
        dofn (fn [f & args] (apply f args))
        parse-line (fn [comps] (map dofn (map (comp swallow-exception letter) format) comps))
        lines (str/split-lines (slurp file))]
    (for [line lines]
      (->> line
           (re-seq #"([^,]+),?")
           (map second)
           parse-line))))

;;; Some tests

(def d (sort-by #(- (nth % 4) (nth % 5))
                (remove (partial some nil?)
                        (read-csv "out/dists.csv"
                                  "sffffffffffff"))))

(defn vline [x]
  (fst/with-ctx [height]
    [:line {:y1 0 :y2 height
            :x1 x :x2 x
            :stroke-width 1 :stroke "#000"}]))

(defn hline [y]
  (fst/with-ctx [width]
    [:line {:x1 0 :x2 width
            :y1 y :y2 y
            :stroke-width 1 :stroke "#000"}]))

(defn axes [& {:keys [x y]}]
  (concat (and x (list (vline x)))
          (and y (list (hline y)))))

(defn sign [^double x]
  (cond (< x 0) -1
        (> x 0) 1
        true 0))

(defn scatter
  ([xs ys & {:keys [radii colors] :or {radii 1 colors "#000"}}]
     (let [n  (min (count xs) (count ys))
           mklst (fn [x] (if (sequential? x) x (list x)))
           xs (mklst xs)
           ys (mklst ys)
           rs (mklst radii)
           cs (mklst colors)]
       (take n
             (map (fn [x y r c]
                    [:circle {:cy y :cx x :r r :fill c :fill-opacity 0.5}])
                  (cycle xs)
                  (cycle ys)
                  (cycle rs)
                  (cycle cs))))))

(defn scatterplot [xs ys rs]
  (let [colors {-1 "#D8B365"
                0 "#333"
                1 "#5AB4AC"}]
    (frame
      (fst/with-ctx! [width height]
        [:rect {:width width :height height :stroke "#000" :stroke-width 0.2}])
      (padded [6]
        (dataframe [tx (scale/affine :domain [0.5 1] :range :x)
                    ty (scale/affine :domain [0.5 1] :range :y)
                    tr (scale/sqrt :domain rs :range [1 2])]
          (scatter (map tx xs)
                   (map ty ys)
                   :radii (map tr rs)
                   :colors (map (comp colors sign -) ys xs)))))))

(defn text-center [txt]
  (dataframe [tx (scale/affine :range :x)
              ty (scale/affine :range :y)]
    [:text {:x (tx 0.5) :y (ty 0.5)
            :fill "#000"
            :font-family "Gill Sans"
            :text-anchor "middle"
            :style (css :dominant-baseline "central")}
     (subs (str txt) 1)]))

(spit "out/test.html"
      (helpers/html5 {:xml? true :mode :xml}
        [:head [:title "Test"]]
        [:body
         [:div {:style (css :width "750px"
                            :height "750px"
                            :margin [0 "auto"]
                            :background "#fafafa")}
          (let [n (count d)
                ml  (map (fn [row] (nth row 3)) d)
                ne  (map (fn [row] (nth row 1)) d)
                np  (map (fn [row] (nth row 2)) d)   
                ep     (map (fn [row] (nth row 4)) d)
                kp     (map (fn [row] (nth row 5)) d)
                e2p    (map (fn [row] (nth row 6)) d)
                epknn2 (map (fn [row] (nth row 7)) d)
                epknn8 (map (fn [row] (nth row 8)) d)
                eps10  (map (fn [row] (nth row 9)) d)
                kpknn2 (map (fn [row] (nth row 10)) d)
                kpknn8 (map (fn [row] (nth row 11)) d)
                kps10  (map (fn [row] (nth row 12)) d)]
            (run-frames {:width 750 :height 750}
              (clipframe
               (padded [20]
                 (vflow
                  (hflow (text-center :ep)
                         (scatterplot ep kp np)
                         (scatterplot ep epknn2 np)
                         (scatterplot ep epknn8 np)
                         (scatterplot ep kpknn2 np)
                         (scatterplot ep kpknn8 np))
                  (hflow (frame)
                         (text-center :kp)
                         (scatterplot kp epknn2 np)
                         (scatterplot kp epknn8 np)
                         (scatterplot kp kpknn2 np)
                         (scatterplot kp kpknn8 np))
                  (hflow (frame)
                         (frame)
                         (text-center :epknn2)
                         (scatterplot epknn2 epknn8 np)
                         (scatterplot epknn2 kpknn2 np)
                         (scatterplot epknn2 kpknn8 np))
                  (hflow (frame)
                         (frame)
                         (frame)
                         (text-center :epknn8)
                         (scatterplot epknn8 kpknn2 np)
                         (scatterplot epknn8 kpknn8 np))
                  (hflow (frame)
                         (frame)
                         (frame)
                         (frame)
                         (text-center :kpknn2)
                         (scatterplot epknn2 kpknn8 np))
                  (hflow (frame)
                         (frame)
                         (frame)
                         (frame)
                         (frame)
                         (text-center :kpknn8)))))))]]))