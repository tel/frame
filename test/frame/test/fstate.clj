(ns frame.test.fstate
  (:use [clojure.test]
        [frame.fstate]
        [clojure.algo.monads  :exclude [state-m update-state set-state
                                        fetch-state fetch-val update-val set-val]]))

(deftest test-state-state-monad
  (testing "Basic context usage analogue w/ built-in state monad"
    (is (= ((domonad state-m
              [_ (set-val :height 30)
               q (fetch-val :height)]
              (+ 1 q)) {:height 1})
           [31 {:height 30}])))
  (testing "Basic state monad activities"
    (is (= (run-state (with-monad state-m (fetch-state)) {:height 3})
           [{:height 3} {:height 3}]))
    (is (= (run-state (with-monad state-m (set-val :height 10)) {:height 3})
           [3 {:height 10}]))
    (is (= (run-state (domonad state-m [] 30) {:height 3})
           [30 {:height 3}]))
    (is (= (run-state (domonad state-m [_ (set-val :height 0)] 30) {:height 3})
           [30 {:height 0}])))
  (testing "Basic context usage analogue"
    (is (= (run-state
            (domonad state-m
              [_ (set-val :height 30)
               q (fetch-val :height)]
              (+ 1 q)) {:height 1})
           [31 {:height 30}]))))

(deftest test-bubble-sfn
  (testing "Should not perturb normal values"
    (is (= (eval-state (bubble-sfn 3)) 3))
    (is (= (eval-state (bubble-sfn [3])) [3]))
    (is (= (eval-state (bubble-sfn (list 3))) (list 3)))
    (is (= (eval-state (bubble-sfn {:a 3})) {:a 3})))
  (testing "Should promote stateful values"
    (let [comp (with-monad state-m (fetch-state))] 
      (is (= (eval-state (bubble-sfn comp)) {}))
      (is (= (eval-state (bubble-sfn [comp])) [{}]))
      (is (= (eval-state (bubble-sfn (list comp))) (list {})))
      (is (= (eval-state (bubble-sfn {:a comp})) {:a {}}))))
  (testing "Should promote stateful values from deep structures"
    (let [comp (with-monad state-m (fetch-state))
          embed (fn [x] [1 2 3 [4 5 6 [7 8 [9 x]]]])] 
      (is (= (eval-state (bubble-sfn (embed comp))) (embed {}))))))

(deftest test-ctx-highlevel
  (testing "Basic context usage"
    (is (= (run-state
            (set-ctx [height 30]
              ;; Note that height isn't available until with-ctx
              ;; unwraps it. The names a misleading and should be
              ;; changed.
              (with-ctx [height]
                (+ 1 height)))
            {:height 1})
           ;; Note that there are no extra layers of wrapping induced
           ;; by set/with-ctx. Those have to be done by frames.
           [31 {:height 30}]))))