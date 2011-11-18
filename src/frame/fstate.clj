(ns frame.fstate
  (use [clojure.algo.monads :exclude [state-m update-state set-state
                                      fetch-state fetch-val update-val set-val]]
       [slingshot.slingshot :only [throw+]]))

;;; Drawing context provided via a stateful monadic computation
;;;

;;; Since SVG always prefers absolute coordinates it's difficult to
;;; programmatically lay out a design. this capability is utterly
;;; fundamental to Frame's goals, though.
;;;
;;; The original implementation used dynamic binding to provide a
;;; *ctx* variable containing height and width information. This
;;; works, but it is very difficult to control evaluation or to track
;;; the current context. In particular, I couldn't find a way to
;;; implement flexible flows since I wouldn't necessarily know the
;;; number of contexts to flow prior to evaluating the flow. It could
;;; be done in a temporary context and recomputed, but that would
;;; involve an exponential blowup in the number of times the Frame
;;; would be computed.

(defn state-m? [obj]
  (-> obj meta :state-m))

(defn run-state
  ([obj] (run-state obj {}))
  ([obj state]
     (if (state-m? obj)
       (obj state)
       obj)))

(defn eval-state [& things] (first (apply run-state things)))

(defmacro sfn [& forms]
  `(with-meta (fn ~@forms) {:state-m true}))

(defmonad state-m
  "Monad describing stateful computations. The monadic values have the
  structure (fn [old-state] [result new-state])."
  [m-result (fn m-result-state [v]
              (sfn [s] [v s]))
   m-bind   (fn m-bind-state [mv f]
              (sfn [s]
                (let [[v ss] (mv s)]
                  ((f v) ss))))])

;;; Rebuild usual state interface to respect the meta tag

(defn update-state
  "Return a state-monad function that replaces the current state by the
   result of f applied to the current state and that returns the old state."
  [f]
  (sfn [s] [s (f s)]))

(defn set-state
  "Return a state-monad function that replaces the current state by s and
   returns the previous state."
  [s]
  (update-state (fn [_] s)))

(defn fetch-state
  "Return a state-monad function that returns the current state and does not
   modify it."
  []
  (update-state identity))

(defn fetch-val
  "Return a state-monad function that assumes the state to be a map and
   returns the value corresponding to the given key. The state is not modified."
  [key]
  (domonad state-m
    [s (fetch-state)]
    (key s)))

(defn update-val
  "Return a state-monad function that assumes the state to be a map and
   replaces the value associated with the given key by the return value
   of f applied to the old value. The old value is returned."
  [key f]
  (sfn [s]
    (let [old-val (get s key)
	  new-s   (assoc s key (f old-val))]
      [old-val new-s])))

(defn set-val
  "Return a state-monad function that assumes the state to be a map and
   replaces the value associated with key by val. The old value is returned."
  [key val]
  (update-val key (fn [_] val)))


;;; Create some context control mechanisms using this monad

(defn bubble-sfn
  "Bubbles a stateful computations upward, consuming and preserving
  intermediate lists, vectors, and hashes."
  [x]
  (with-monad state-m
    (cond (sequential? x)
          (let [refn (m-lift 1 (cond (vector? x) vec
                                     (set? x) set
                                     true identity))]
            (->> x
                 (map bubble-sfn)
                 m-seq refn))
          
          (map? x)
          ((m-lift 1 (partial into {})) (bubble-sfn (seq x)))

          :else
          (if (state-m? x)
            x (m-result x)))))

(defmacro with-ctx
  ([syms form] `(with-ctx ~syms nil ~form))
  ([syms nil-v form]
     `(domonad state-m
        [~(if (map? syms)
            syms
            ;; Use a map destructuring bind between mappings in the state
            ;; and the local vars.
            (merge (apply merge (map (fn [sym] {sym (keyword sym)}) syms))
                   {:or (apply merge (map (fn [sym] {sym nil-v}) syms))}))
         (fetch-state)
         res# (bubble-sfn
               ;; Note we revert to the identity monad so that the
               ;; state-m interface isn't accidentally exposed
               (with-monad identity-m ~form))]
        res#)))

(defmacro with-ctx!
  "Normally with-ctx just silently nils any requested symbols which
  aren't in the current state context. with-ctx! modifies this
  behavior to reject unset"
  [syms form]
  `(with-ctx ~syms ::undefined
     (if (some #(= ::undefined %) ~syms)
       (throw+ {:fatal? true
                :message (apply str "No contextual binding provided for symbols: "
                                (interpose
                                 " "
                                 (map first
                                      (filter (comp #(= ::undefined %) second)
                                              (map list
                                                   '[~@syms]
                                                   ~syms)))))})
       ~form)))

(defn set-ctx*
  "A definitive functional form of set-ctx that uses keywords. All the
  macro does is translate the symbols unevaluated to keywords"
  [bindings form]
  (with-monad state-m
    (m-bind (m-seq (map #(apply set-val %) (partition 2 (flatten (seq bindings)))))
            (constantly (bubble-sfn form)))))

(defmacro set-ctx [bindings form]
  `(set-ctx* ~(vec (map-indexed
                    (fn [i x]
                      (if (even? i) (keyword x) x)) bindings))
             (with-monad identity-m
               ~form)))

(defn update-ctx* [args-and-fns form]
  (with-monad state-m
    (m-bind (m-seq (map #(apply update-val %) (partition 2 (flatten (seq args-and-fns)))))
            (constantly (bubble-sfn form)))))

(defmacro update-ctx [args-and-fns form]
  `(update-ctx* ~(vec (map-indexed
                       (fn [i x]
                         (if (even? i) (keyword x) x)) args-and-fns))
                (with-monad identity-m
                  ~form)))