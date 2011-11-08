(ns frame.core
  (:use hiccup.core)
  (:use [clojure.core.match :only [match]]))

(defn test [vec]
  (html [:html {:xmlns:svg "http://www.w3.org/2000/svg"}
         [:head [:title "Test"]]
         [:body
          [:div {:style "width: 800px; margin: 0 auto; background: #eee"}
           vec]]]))

(defn svg* [gen-body]
  ;; Eventually we'll parse down for sanity in dimensions
  (binding [*ctx* (merge *ctx* {:height 500 :width 800})]
    [:svg {:width 800 :height 500} (gen-body)]))

(defmacro svg [& body]
  `(svg* (fn [] ~@body)))

(defmacro testsvg [vecs]
  `(spit "out/test.html" (test (svg ~vecs))))

;;; -----

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
                    "(" (concat (interpose ", " (map to-fexpr
                                                     rest)) [")"]))
             (apply str (interpose " " (map to-fexpr form))))
           (str head)))
       (str form))))

(defn css
  "Convert a hash to a css style declaration using to-f."
  ([& keys]
     (let [hash (apply hash-map keys)]
       (apply str
              (for [[k v] hash]
                (str (subs (str k) 1) ": "
                     (if (sequential? v) (to-fexpr v) v)
                     "; "))))))

(defn merge-valued
  ([map1 map2]
     (reduce
      (fn [map [k v]]
        (if v (assoc map k v) map))
      map1
      map2))
  ([map1 map2 & maps] (reduce merge-valued (merge-valued map1 map2) maps)))

(def ^{:dynamic true
       :doc "Current rendering context."}
  *ctx* {})

(defn axes []
  [:g {:id "axes"}
         (apply concat
                (for [i (range 100)]
                  (let [pos (- (* 20 i) 1000)]
                    [[:circle {:r 1 :cy 0 :cx pos}]
                     [:text {:y 5 :x (- (* 20 i) 1000)
                             :style (css :font-size 8)} pos]])))
         (apply concat
                (for [i (range 100)]
                  (let [pos (- (* 20 i) 1000)]
                    [[:circle {:r 1 :cx 0 :cy pos}]
                     [:text {:x 5 :y (- (* 20 i) 1000)
                             :style (css :font-size 8)} pos]])))])

(comment
  ;; An example for how this is supposed to work
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
  ;; Dotplot
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
  )

(defn canvas* [args gen-children]
  (let [{:keys [width height xlim ylim xtransform ytransform]
         :or {width  (:width  *ctx*)
              height (:height *ctx*)
              xtransform identity
              ytransform identity}}
        args

        [xmin xmax] (or xlim [0 width])
        xbound (- xmax xmin)
        [ymin ymax] (or ylim [0 height])
        ybound (- ymax ymin)
        name (gensym "boundary")]
    (binding
        [*ctx* (merge *ctx*
                      {:transform-x (fn [x] (* (/ x xbound) width))
                       :transform-y (fn [y] (- (* (/ y ybound) height)))
                       :ybound ybound})]
      [:g
       [:defs [:rect (merge-valued {:id (str name)}
                                   {:height height :width width})]
        (axes)]
       (vec (concat [:g {;; :style (css :clip-path name)
                         :transform (to-f [:translate 0 420]
                                          [:scale 1 1])}
                     [:use {:xlink:href (to-f [:url "#axes"])}]
                     ]
                    (gen-children)))])))

(defmacro canvas [args & children]
  `(canvas* ~args (fn [] ~@children)))

(defn point [x y]
  (let [tx (:transform-x *ctx* identity)
        ty (:transform-y *ctx* identity)]
    [:circle {:r 1 :cx (float (tx x)) :cy (float (ty y))}]))

(testsvg
 (canvas {:xlim [0 50] :ylim [0 (* 50 50)]}
   (println *ctx*)
   (for [i (range 50)]
     (point i (* i i)))))