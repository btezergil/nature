(ns nature.oracle-test
  (:require [clojure.spec.alpha :as s]
            [nature.core :as nature]
            [nature.population-presets :as pp]
            [nature.spec :as spec]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(defn species [id size overrides]
  (let [counter (atom 0)]
    (merge {:species-id id :population-size size
            :genome-generator #(vector id (swap! counter inc))
            :binary-operators [] :unary-operators [] :carry-over 0 :insert-new 0}
           overrides)))

(def references {:a {:reference-id "a-exit" :policy :best-favorable}
                 :b {:reference-id "b-exit" :policy :first-favorable}})
(defn options []
  {:collaboration-mode :oracle
   :oracle-fitness-fns {:a (fn [[_ n]] n) :b (fn [[_ n]] (- n))}
   :oracle-reference-metadata references
   :final-evaluation-fn (fn [a b] {:pair [a b]})})
(defn run-zero [opts]
  (nature/evolve-cooperatively (species :a 3 {}) (species :b 2 {}) 0 nil opts))

(deftest oracle-independent-fitness-and-evidence-test
  (let [calls (atom [])
        final-calls (atom [])
        states (atom [])
        result (run-zero (assoc (options)
                               :oracle-fitness-fns
                               {:a (fn [[_ n :as genome]] (swap! calls conj [:a genome]) n)
                                :b (fn [[_ n :as genome]] (swap! calls conj [:b genome]) (- n))}
                               :final-ratio 0.5
                               :final-evaluation-fn (fn [a b] (swap! final-calls conj [a b]) {:pair [a b]})
                               :monitors [#(swap! states conj %)]))]
    (is (= [1 2 3] (mapv :fitness-score (get-in result [:populations :a]))))
    (is (= [-1 -2] (mapv :fitness-score (get-in result [:populations :b]))))
    (is (= {:a 3 :b 2} (frequencies (map first @calls))))
    (is (= 5 (:oracle-evaluation-count result)
           (:directional-collaboration-count result)
           (:unique-collaboration-evaluation-count result)))
    (is (= references (:oracle-reference-metadata result)))
    (is (= 1 (count @states)))
    (is (s/valid? ::spec/coevolution-state (first @states)))
    (is (s/valid? ::spec/coevolution-result result))
    (is (not (contains? result :panels)))
    (is (not (contains? result :panel-history)))
    (doseq [row (:collaborations result)]
      (is (= :oracle (:reference-kind row)))
      (is (= (:reference-id row) (get-in references [(:focal-species-id row) :reference-id])))
      (is (= {(:focal-species-id row) (:focal-guid row)} (:participants row)))
      (is (not (contains? row :collaborator-guid)))
      (is (= #{(:focal-species-id row)} (set (keys (:genomes row))))))
    (testing "ordinary final pairs follow each species' independently assigned fitness"
      (is (= [[:a 3] [:a 2]] (mapv :genetic-sequence (get-in result [:solutions :a]))))
      (is (= [[:b 1]] (mapv :genetic-sequence (get-in result [:solutions :b]))))
      (is (= [[[:a 3] [:b 1]] [[:a 2] [:b 1]]] @final-calls))
      (is (= 2 (count (:final-collaborations result)))))))

(deftest oracle-reproduction-and-re-evaluation-test
  (let [states (atom []) calls (atom []) operators (atom [])
        config (fn [id]
                 (species id 3
                          {:carry-over 1 :insert-new 1
                           :binary-operators [(fn [a b]
                                                (swap! operators conj [id a b])
                                                [[id 10]])]
                           :unary-operators [(fn [[tag n]] [tag (inc n)])]}))
        scorer (fn [id offset]
                 (fn [genome] (swap! calls conj [id genome]) (+ offset (second genome))))
        result (nature/evolve-cooperatively
                (config :a) (config :b) 2 nil
                (assoc (options) :oracle-fitness-fns {:a (scorer :a 100) :b (scorer :b -100)}
                       :monitors [#(swap! states conj %)]))]
    (is (= [0 1 2] (mapv :generation @states)))
    (is (= 18 (count @calls)))
    (is (= 4 (count @operators)))
    (doseq [[id a b] @operators]
      (is (= id (first a) (first b))))
    (doseq [state @states]
      (is (s/valid? ::spec/coevolution-state state))
      (is (= references (:oracle-reference-metadata state)))
      (is (= #{:a :b} (set (keys (:populations state)))))
      (is (= 6 (:oracle-evaluation-count state)))
      (doseq [[id population] (:populations state) individual population]
        (is (= (+ (if (= :a id) 100 -100) (second (:genetic-sequence individual)))
               (:fitness-score individual)))))
    (doseq [[previous next-state] (partition 2 1 @states) id [:a :b]]
      (let [old (get-in previous [:populations id])
            population (get-in next-state [:populations id])
            elite (apply max-key :fitness-score old)
            carried (first (filter #(= (:guid elite) (:guid %)) population))
            child (first (filter #(and (zero? (:age %))
                                       (not= pp/initializer-name (:parents %))) population))]
        (is (= (inc (:age elite)) (:age carried)))
        (is (= (:genetic-sequence elite) (:genetic-sequence carried)))
        (is (every? (set (map :guid old)) (:parents child)))
        (is (= 2 (count (:parents child))))))
    (is (s/valid? ::spec/coevolution-result result))))

(deftest oracle-credit-policy-test
  (doseq [policy [:mean :maximum :top-two-mean :weighted]
          :let [opts (cond-> (assoc (options) :credit-policy policy)
                       (= policy :weighted) (assoc :credit-weights [0.4 0.3]))]]
    (let [result (run-zero opts)]
      (is (= policy (:credit-policy result)))
      (is (every? true? (map == [1 2 3] (map :fitness-score (get-in result [:populations :a])))))
      (is (every? true? (map == [-1 -2] (map :fitness-score (get-in result [:populations :b])))))))
  (let [contexts (atom [])
        result (run-zero (assoc (options) :credit-policy
                                (fn [context]
                                  (swap! contexts conj context)
                                  (* 2 (:score (first (:encounters context)))))))]
    (is (= 5 (count @contexts)))
    (is (= [2 4 6] (mapv :fitness-score (get-in result [:populations :a]))))
    (doseq [context @contexts]
      (is (= :oracle (:collaboration-mode context)))
      (is (= (get references (:species-id context)) (:oracle-reference context)))
      (is (= 1 (count (:encounters context)))))
    (is (= {:fitness-score 2 :average-score 1 :maximum-score 1 :encounter-count 1}
           (get-in result [:oracle-statistics :a
                           (get-in result [:populations :a 0 :guid])])))))

(deftest oracle-validation-before-initialization-test
  (doseq [invalid [(dissoc (options) :oracle-fitness-fns)
                   (assoc (options) :oracle-fitness-fns {:a identity})
                   (assoc (options) :oracle-fitness-fns {:a identity :b 3})
                   (assoc (options) :oracle-fitness-fns {:a identity :b identity :extra identity})
                   (dissoc (options) :oracle-reference-metadata)
                   (assoc (options) :oracle-reference-metadata {:a {:reference-id "x"}})
                   (assoc-in (options) [:oracle-reference-metadata :b :reference-id] " ")
                   (assoc-in (options) [:oracle-reference-metadata :b :reference-id] 1)
                   (dissoc (options) :final-evaluation-fn)
                   (assoc (options) :final-evaluation-fn 1)
                   (assoc (options) :opponents 1)
                   (assoc (options) :panel-selection-fns [])]]
    (let [initialized (atom 0)
          generator #(do (swap! initialized inc) [:genome])]
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                   (nature/evolve-cooperatively
                    (species :a 1 {:genome-generator generator})
                    (species :b 1 {:genome-generator generator}) 0 nil invalid)))
      (is (zero? @initialized)))))

(deftest oracle-options-do-not-leak-into-other-modes-test
  (doseq [mode [:balanced :cartesian :panel]
          key [:oracle-fitness-fns :oracle-reference-metadata]]
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"Oracle options require"
         (nature/evolve-cooperatively (species :a 1 {}) (species :b 1 {}) 0 (constantly 1)
                                     {:collaboration-mode mode key (get (options) key)}))))
  (doseq [mode [:balanced :cartesian :panel]]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                 (nature/evolve-cooperatively (species :a 1 {}) (species :b 1 {}) 0 nil
                                             {:collaboration-mode mode})))))

(deftest oracle-failure-context-test
  (doseq [bad-score [nil "bad" ##NaN ##Inf ##-Inf]]
    (try
      (run-zero (assoc-in (options) [:oracle-fitness-fns :a] (constantly bad-score)))
      (is false "non-finite or non-numeric oracle scores must fail")
      (catch #?(:clj Exception :cljs :default) e
        ;; JVM pmap propagates callback failures through deref's wrapper.
        (let [cause #?(:clj (loop [cause e]
                             (if (.getCause cause) (recur (.getCause cause)) cause))
                       :cljs e)
              data (ex-data cause)]
          (is (= 0 (:generation data)))
          (is (= :a (:species-id data)))
          (is (= "a-exit" (:reference-id data)))
          (is (string? (:focal-guid data)))))))
  (try
    (run-zero (assoc-in (options) [:oracle-fitness-fns :b]
                       (fn [_] (throw (ex-info "domain failure" {:domain :probe})))))
    (is false "oracle callback exceptions must retain diagnostic context")
    (catch #?(:clj Exception :cljs :default) e
      (let [context #?(:clj (loop [cause e]
                             (if (:focal-guid (ex-data cause))
                               (ex-data cause)
                               (when-let [next-cause (.getCause cause)] (recur next-cause))))
                       :cljs (ex-data e))]
        (is (= :b (:species-id context)))
        (is (= :probe (:domain context)))))))

(deftest oracle-spec-rejects-inconsistent-evidence-test
  (let [result (run-zero (options))]
    (doseq [bad [(assoc result :oracle-evaluation-count 0)
                 (assoc-in result [:collaborations 0 :reference-id] "wrong")
                 (assoc-in result [:collaborations 0 :collaborator-guid] "fake-live-reference")
                 (assoc-in result [:collaborations 0 :score] ##NaN)
                 (update result :oracle-reference-metadata dissoc :b)]]
      (is (not (s/valid? ::spec/coevolution-result bad))))))

(deftest oracle-never-calls-pair-fitness-during-evolution-test
  (let [result (nature/evolve-cooperatively
                (species :a 2 {}) (species :b 1 {}) 0
                (fn [_ _] (throw (ex-info "pair callback used during oracle evolution" {})))
                (options))]
    (is (= 3 (:oracle-evaluation-count result)))
    (is (= 2 (count (:final-collaborations result))))))

#?(:clj
   (deftest oracle-evaluation-is-parallel-test
     (let [active (atom 0) maximum (atom 0)
           scorer (fn [_]
                    (let [count (swap! active inc)]
                      (swap! maximum max count)
                      (Thread/sleep 50)
                      (swap! active dec)
                      1))]
       (run-zero (assoc (options) :oracle-fitness-fns {:a scorer :b scorer}))
       (is (> @maximum 1)))))
