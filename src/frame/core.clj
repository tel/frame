(ns frame.core
  (:use hiccup.core)
  (:require [hiccup.page-helpers :as helpers]
            [frame.fstate :as fst]
            [clojure.algo.monads :as m]
            [frame.scale :as scale]
            [tappan.matrix :as mat]
            [tappan.workers.kmeans_purity :as km]
            [clojure.string :as str])
  (:gen-class))

;;; Some XML and SVG Hiccuphelpers
;;;
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

(defn doframes [ctx frame]
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

(defn padded*
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

(defmacro padded [padding & forms]
  `(padded* ~padding (list ~@forms)))


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
                    [:circle {:cy y :cx x :r r :fill c :fill-opacity 0.7}])
                  (cycle xs)
                  (cycle ys)
                  (cycle rs)
                  (cycle cs))))))

(defn scatterplot [xs ys rs & {:keys [xlim ylim colors]
                               :or {colors [0]}}]
  (let [color-map {-1 "#D8B365"
                   0 "#888"
                   1 "#5AB4AC"}]
    (padded [4]
      (dataframe [tx (scale/affine :domain (or xlim xs) :range :x)
                  ty (scale/affine :domain (or ylim ys) :range :y)
                  tr (scale/sqrt :domain rs :range [0.2 2.2])]
        (scatter (map tx xs)
                 (map ty ys)
                 :radii (map tr rs)
                 :colors (map color-map colors))))))

(defn text-center [txt]
  (dataframe [tx (scale/affine :range :x)
              ty (scale/affine :range :y)]
    [:text {:x (tx 0.5) :y (ty 0.5)
            :fill "#444"
            :font-family "Gill Sans"
            :font-size "10pt"
            :text-anchor "middle"
            :style (css :dominant-baseline "central")}
     (str txt)]))

(defn test-html [frame]
  (helpers/html5 {:xml? true :mode :xml}
                 [:head [:title "Test"]]
                 [:body
                  [:div {:style (css :width "1100px"
                                     :height "750px"
                                     :margin [0 "auto"]
                                     :background "#fafafa")}
                   frame]]))

(defn test-svg [frame]
  (html frame))

(defn dogrid* [xlist ylist fn]
  (vflow (for [x xlist]
           (hflow (for [y ylist]
                    (fn x y))))))

(defmacro dogrid [[xsym xs ysym ys] & body]
  `(dogrid* ~xs ~ys (fn [~xsym ~ysym] ~@body)))

(defn svg-matrix [m]
  (fst/with-ctx! [width height]
    (let [[n d] (mat/size m)]
      (dataframe [tc (scale/bw :domain (mat/gets* m :_ :_))
                  ox (scale/affine :domain (range d) :range :x)
                  oy (scale/affine :domain (range n) :range :y :flip? true)]
        (for [i (range n) j (range d)]
          [:rect {:width (ox 1) :height (oy 1)
                  :x (ox j) :y (oy i)
                  :fill (tc (mat/get m i j))}])))))

(defn -main [& args]
  (let [d (sort-by #(- (nth % 4) (nth % 5))
                   (remove (partial some nil?)
                           (read-csv "out/dists.csv"
                                     "sffffffffffffffffff")))
        n (count d)
        ml  (map (fn [row] (nth row 3)) d)
        ne  (map (fn [row] (nth row 1)) d)
        np  (map (fn [row] (nth row 2)) d)
        ep     (map (fn [row] (nth row 4)) d)
        epknn2 (map (fn [row] (nth row 5)) d)
        epknn8 (map (fn [row] (nth row 6)) d)
        eps10  (map (fn [row] (nth row 7)) d)
        eps5   (map (fn [row] (nth row 8)) d)
        kp     (map (fn [row] (nth row 9)) d)
        kpknn2 (map (fn [row] (nth row 10)) d)
        kpknn8 (map (fn [row] (nth row 11)) d)
        kps10  (map (fn [row] (nth row 12)) d)
        kps5   (map (fn [row] (nth row 13)) d)
        base   (map (fn [row] (nth row 14)) d)]
    (spit
     "out/test.html"
     (test-html
      (doframes {:width 1100 :height 750}
        (clipframe
          (padded [20]
            (let [comps [base
                         ep eps10 eps5 epknn2 epknn8
                         kp kps10 kps5 kpknn2 kpknn8]
                  syms '[base
                         ep eps10 eps5 epknn2 epknn8
                         kp kps10 kps5 kpknn2 kpknn8]
                  n (count comps)]
              (dogrid [i (range n) j (range n)]
                (cond (< j i)  (frame)
                      (== j i) (text-center (nth syms i))
                      :else    (let [x (nth comps i)
                                     y (nth comps j)]
                                 (scatterplot ml (map - x y) np
                                              :ylim [-0.4 0.4]
                                              :colors (map (comp sign -) x y))
                                 ;; (scatterplot y x ne
                                 ;;              :xlim [0.5 1]
                                 ;;              :ylim [0.5 1]
                                 ;;              :colors (map (comp sign -) x y))
                                 )))))))))))

;; (let [aff (mat/map #(Math/exp (/ (* -1 % %) (* 2 30 30))) di)
;;       [n d] (mat/size aff)
;;            root-d (mat/diag
;;                    (for [i (range n)]
;;                      (/ (Math/sqrt
;;                          (reduce
;;                           + (mat/gets* aff i :_))))))
;;       lap (mat/prod root-d aff root-d)
;;       evec (km/espace-of aff 2)
;;       dist (mat/prod evec (mat/t evec))]
;;   (spit "out/dists.svg"
;;         (test-svg
;;          (doframes {:width 1200 :height 300}
;;            (frame
;;             (hflow
;;              (padded [20] (svg-matrix di))
;;              (padded [20] (svg-matrix aff))
;;              (padded [20] (svg-matrix lap))
;;              (padded [20] (svg-matrix dist))))))))