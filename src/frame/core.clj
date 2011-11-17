(ns frame.core
  (:use hiccup.core)
  (:require [hiccup.page-helpers :as helpers]
            [clojure.string :as str]))

;;; Working with a dynamic context
;;; 
(def ^{:dynamic true
       :doc "Current rendering context."}
  *ctx* nil)

(defn- ctx-has? [& syms]
  (every? #(% *ctx*) syms))

(defmacro set-ctx [args & body]
  `(binding [*ctx* (merge *ctx* ~args)]
     ~@body))

(defmacro update-ctx [args-and-fns & body]
  `(binding [*ctx* (reduce
                    (fn [ctx# [key# fn#]]
                      (update-in ctx# [key#] fn#))
                    *ctx*
                    ~args-and-fns)]
     ~@body))

(defmacro with-ctx [syms & body]
  (let [let-forms
        (if (map? syms)
          `[~syms *ctx*]
          (vec (apply concat
                      (map (fn [sym]
                             `[~sym ((keyword '~sym) *ctx*)])
                           syms))))
        syms (if (map? syms)
               (map (fn [[_ sym]] sym) syms)
               syms)]
    `(if (ctx-has? ~@(map keyword syms))
       (let ~let-forms
         ~@body)
       (throw
        (Exception.
         (str "Insufficient context! Need "
              '~syms
              ". Context is "
              (pr-str *ctx*)))))))

;;; Some XML and SVG Hiccuphelpers
;;; 
(def doctype (merge helpers/doctype
                    {:svg "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"}))

(def xml?
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>")

(defn svg-tag [attrs body]
  [:svg (merge {:xmlns "http://www.w3.org/2000/svg"
                :xmlns:xlink "http://www.w3.org/1999/xlink"
                :version "1.1"} attrs) body])

(defn svg* [args gen-body]
  ;; Eventually we'll parse down for sanity in dimensions
  (set-ctx args
    (svg-tag {:width (:width *ctx*) :height (:height *ctx*) :fill :none} (gen-body))))

(defmacro svg [args & body]
  `(svg* ~args (fn [] ~@body)))

(defn- to-f
  "Convert some Hiccup-like vectors to sequences of f-exprs."
  ([form & forms] (to-f (cons form forms)))
  ([form]
     (if (sequential? form)
       (let [[head & rest] form]
         (if (not (empty? rest))
           (if (keyword? head)
             (apply str
                    (subs (str (first form)) 1)
                    "(" (concat (interpose ", " (map to-f
                                                     rest)) [")"]))
             (apply str (interpose " " (map to-f form))))
           (str head)))
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
(defn spy [x] (prn x) x)
(defn- is-hiccup-vec
  [[x & _]] (not (sequential? x)))

(defn- flatten-hiccup* [vs]
  (loop [vs vs build nil]
    (let [[v & vs] vs]
     (if (nil? v)
       (vec build)
       (if (is-hiccup-vec v)
         (recur vs (cons v build))
         (recur vs (concat (flatten-hiccup* v) build)))))))

(defn- flatten-hiccup [x]
  (if (sequential? x)
    (if (is-hiccup-vec x) [x]
        (vec (reverse (flatten-hiccup* x))))
    x))

(defn frame* [args gen-children]
  (set-ctx args
    (if (ctx-has? :width :height)
      (with-ctx [width height]
        (let [clipname (str (gensym))
              inner (fn []
                      [:g
                       [:defs
                        [:rect {:id clipname :width width :height height}]]
                       `[:g ~{:style (css :clip-path (to-f [:url (str "#" clipname)]))}
                         ~@(flatten-hiccup (gen-children))]])]
          ;; TODO: Figure out why clipping breaks when I don't create
          ;; a new SVG element. It's a bit clunky to keep creating
          ;; extra layers.
          (if (:svg *ctx*)
            (inner)
            (svg {}
                 (set-ctx {:svg true}
                   (inner))))))
      (throw
       (Exception.
        "Outermost frame needs to specify the :height and :width.")))))

(defmacro frame [args & body]
  `(frame* ~args (fn [] [~@body])))

(defn- expand-margin
  "Expand the SVG margin syntax."
  ([a] [a a a a])
  ([a b] [a b a b])
  ([a b c d] [a b c d]))

(defmacro padded
  [padding & rest]
  `(let [[a# b# c# d#] (expand-margin ~@padding)
         dy# (+ a# c#)
         dx# (+ b# d#)]
     ;; Shrink the inner context
     (update-ctx {:width #(- % dx#)
                  :height #(- % dy#)}
       ;; And transform it away from the upper left corner
       [:g {:transform (to-f [:translate d# a#])}
        (frame {} ~@rest)])))

;;; Scales and transformations
;;; 
(defn itransform [g ;; transformation and inverse
                  & {:keys [auto domain range]
                     :or {domain [0 1]
                          range  [0 1]}}]
  (with-ctx [width height]
    (let [auto? (not (nil? auto))
          auto (remove nil? auto)
          ;; Fix the domain to the data using auto
          domain (if auto?
                   [(apply min auto) (apply max auto)]
                   domain)
          ;; Attempt to build the range automatically
          range (case range
                  :x [0 width]
                  :y [height 0]
                  range)
          [in0 inf] domain
          [out0 outf] range
          domain-meas (- inf in0)
          range-meas  (- outf out0)]
      #(float
        (+ (* range-meas
              (/ (g (/ (- % in0)
                       domain-meas))
                 (g 1)))
           out0)))))

(defn scale [g ;; transformation
             & {:keys [auto domain range]
                :or {range  [0 1]}}]
  (with-ctx [width height]
    (let [auto (map g auto)
          auto? (not (empty? auto))
          ;; Fix the domain to the data using auto
          domain (or domain
                     (if auto?
                       [(apply min auto) (apply max auto)])
                     [0 1])
          ;; Attempt to build the range automatically
          range (case range
                  :x [0 width]
                  :y [height 0]
                  range)
          [in0 inf] (if auto? domain (map g domain))
          [out0 outf] range
          domain-meas (- inf in0)
          range-meas  (- outf out0)]
      #(float
        (+ (* range-meas
              (/ (- (g %) in0)
                 domain-meas))
           out0)))))

(defn scale:affine [& args]
  (apply scale identity args))

(defn scale:sqrt [& args]
  (apply scale #(Math/sqrt %) args))

(defn scale:log [& args]
  (let [{:keys [expt]} args
        lbase (and expt (Math/log expt))
        g (if expt
            #(/ (Math/log %) lbase)
            #(if (== % 0)
               (throw (Exception. "Cannot take log of 0."))
               (Math/log %)))]
    (apply scale g args)))

(defn scale:exp [& args]
  (let [{:keys [expt]} args
        g (if expt
               #(Math/pow expt %)
               #(Math/exp %))]
    (apply scale g args)))

;;; Flows

(defn- partition-interval
  "Partition an interval using a flexible interval set."
  [int part])

(defn hflow [])

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
  (with-ctx [height]
    [:line {:y1 0 :y2 height
            :x1 x :x2 x
            :stroke-width 1 :stroke "#000"}]))

(defn hline [y]
  (with-ctx [width]
    [:line {:x1 0 :x2 width
            :y1 y :y2 y
            :stroke-width 1 :stroke "#000"}]))

(defn sign [^double x]
  (cond (< x 0) -1
        (> x 0) 1
        true 0))

(defmacro dataspace [[tx-sym xs ty-sym ys &
                      {:keys [xlim ylim]}]
                     & body]
  `(let [xs# ~xs ys# ~ys xlim# ~xlim ylim# ~ylim]
     (let [~tx-sym (scale:affine :auto xs# :domain xlim# :range :x)
           ~ty-sym (scale:affine :auto ys# :domain ylim# :range :y)]
       ~@body)))

(defn scatter [xs ys rs colors]
  (dataspace [tx xs ty ys :xlim [0.5 1] :ylim [0.5 1]]
    (let [tr (scale:sqrt :auto rs :range [1 6])]
      [(hline (ty 0))
       (for [i (range (count xs))]
         (let [x (nth xs i)
               y (nth ys i)
               r (nth rs i)
               clr (nth colors i)]
           [:circle {:cy (ty y) :cx (tx x) :r (tr r)
                     :fill clr}]))])))


(spit "out/test.html"
      (html
       [:html {:xmlns:svg "http://www.w3.org/2000/svg"}
        [:head [:title "Test"]]
        [:body
         [:div {:style (css :width "500px"
                            :height "500px"
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
                kps10  (map (fn [row] (nth row 12)) d)
                colors {1 "green" 0 "black" -1 "red"}]
            (list
             (frame {:width 500 :height 500}
               (padded [6]
                 (scatter kp kpknn8 ne (map (comp colors sign -) kp kpknn8))))))]]]))