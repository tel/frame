(ns frame.core.bubbles
  (:use clojure.walk
        [slingshot.slingshot :only [throw+]]))

;;; Bubbling is a sneaky way of passing state into lexical contexts
;;; without being too explicit about it.

(defn bubble? [obj]
  (:bubble (meta obj) false))

(defn assert-letlike [name things]
  (when-not (even? (count things))
    (throw+ (java.lang.IllegalArgumentException.
             (str "`" name "` requires an even number of forms in binding vector")))))

(let [state-var (gensym)]
  (defmacro bubble [& body]
    `(with-meta
       (fn [~state-var]
        (postwalk (fn [obj#]
                    (if (bubble? obj#)
                      (obj# ~state-var)
                      obj#))
                  (do ~@body)))
       {:bubble true}))
  
  (defmacro with-ctx [things & body]
    (assert-letlike "with-ctx" things)
    `(let ~(-> (reduce (fn [set [name key]]
                         (cons name
                               (cons `(~key ~state-var) set)))
                       []
                       (partition 2 things))
               vec)
       ~@body))

  (defmacro swap-ctx [fn & body]
    `(let [~state-var (~fn ~state-var)]
       ~@body)))

(defmacro swap-in-ctx [things & body]
  (assert-letlike "swap-in-ctx" things)
  (let [[keys fn & rest] things]
    (if keys
      `(swap-ctx #(update-in % ~(if (vector? keys) keys [keys]) ~fn)
                 (swap-ctx-vars ~(vec rest) ~@body))
      `(do ~@body))))