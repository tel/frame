(ns frame.core
  (:use hiccup.core)
  (:require [hiccup.page-helpers :as helpers]))

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
    (svg-tag {:width (:width *ctx*) :height (:height *ctx*)} (gen-body))))

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
(defn frame* [args gen-children]
  (set-ctx args
    (if (ctx-has? :width :height)
      (with-ctx [width height]
        (let [clipname (str (gensym))
              inner [:g
                     [:defs
                      [:rect {:id clipname :width width :height height}]]
                     [:g {:clip-path (to-f [:url (str "#" clipname)])}
                      (gen-children)]]]
          (if (:svg *ctx*)
            inner
            (svg {}
                 (set-ctx {:svg true}
                   inner)))))
      (throw
       (Exception.
        "Outermost frame needs to specify the :height and :width.")))))

(defmacro frame [args & body]
  `(frame* ~args (fn [] ~@body)))

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
     (update-ctx {:width #(- % dx#)
                  :height #(- % dy#)}
       [:g {:transform (to-f [:translate a# d#])}
        ~@rest])))

;;; Scales and transformations
;;; 
(defn itransform [g ;; transformation and inverse
                  & {:keys [auto domain range]
                     :or {domain [0 1]
                          range  [0 1]}}]
  (with-ctx [width height]
    (let [auto? auto
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
                :or {domain [0 1]
                     range  [0 1]}}]
  (with-ctx [width height]
    (let [auto (map g auto)
          auto? (not (empty? auto))
          ;; Fix the domain to the data using auto
          domain (if auto?
                   [(apply min auto) (apply max auto)]
                   domain)
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
               (Math/log %)))
        ginv (if expt
               #(Math/pow expt %)
               #(Math/exp %))]
    (apply scale g args)))

(defn- partition-interval
  "Partition an interval using a flexible interval set."
  [int part])

(spit "out/test.html"
      (html
       [:html {:xmlns:svg "http://www.w3.org/2000/svg"}
        [:head [:title "Test"]]
        [:body
         [:div {:style "width: 800px; height 500px; margin: 0 auto; background: #eee"}
          (let [data (for [i (map #(/ % 100) (range 1 1000))]
                       {:x i :y (Math/sin (Math/pow i 2))})]
            (frame {:width 800 :height 500}
              (padded [50]
                (let [tx (scale:affine :auto (map :x data) :range :x)
                      ty (scale:affine :auto (map :y data) :range :y)
                      tr (scale:sqrt   :auto (map :x data) :range [0 10])]
                  (for [{:keys [x y]} data]
                    [:circle {:r 1 :cx (tx x) :cy (ty y)}])))))]]]))

