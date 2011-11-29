(ns frame.scale
  (:require [frame.fstate :as fst]
            [clojure.algo.monads :as m]))

(defn scale [g ;; transformation
             & {:keys [domain range flip?]
                :or {range [0 1]}}]
  (fst/with-ctx [width height] 1
    (let [domain (if domain
                   [(apply min domain) (apply max domain)]
                   [0 1])
          ;; Attempt to build the range automatically
          range (case range
                  :x [0 width]
                  :y [height 0]
                  range)
          [in0 inf] (map g domain)
          [out0 outf] (if flip? (reverse range) range)
          domain-meas (- inf in0)
          range-meas  (- outf out0)]
      (if (== 0 domain-meas)
        (constantly (+ (/ range-meas 2) out0))
        #(float
          (+ (* range-meas
                (/ (- (g %) in0)
                   domain-meas))
             out0))))))

(defn affine [& args]
  (apply scale identity args))

(defn sqrt [& args]
  (apply scale #(Math/sqrt %) args))

(defn log [& args]
  (let [{:keys [expt]} args
        lbase (and expt (Math/log expt))
        g (if expt
            #(/ (Math/log %) lbase)
            #(if (== % 0)
               (throw (Exception. "Cannot take log of 0."))
               (Math/log %)))]
    (apply scale g args)))

(defn exp [& args]
  (let [{:keys [expt]} args
        g (if expt
               #(Math/pow expt %)
               #(Math/exp %))]
    (apply scale g args)))

;;; Color scales

(defn bw [& args]
  (m/domonad fst/state-m
    [:let [args (apply merge (apply hash-map args)
                       {:range [0 255]})]
     scale (apply affine (apply concat (seq args)))]
    (fn [v]
      (let [z (Integer/toHexString (Math/floor (scale v)))]
        (str "#" z z z)))))