(ns nature.continuation-test
  (:require [clojure.spec.alpha :as s]
            [nature.core :as nature]
            [nature.initialization-operators :as initialization]
            [nature.population-presets :as pp]
            [nature.spec :as spec]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(defn- species
  ([id n generator]
   {:species-id id :population-size n :genome-generator generator
    :carry-over 0 :insert-new 0 :binary-operators [] :unary-operators []})
  ([id n] (species id n #(throw (ex-info "Unexpected initialization" {:species id})))))

(defn- population [id scores]
  (mapv (fn [n score]
          {:guid (str (name id) "-" n) :genetic-sequence [id n]
           :parents ["earlier-parent"] :age (+ 2 n) :fitness-score score})
        (range 1 (inc (count scores))) scores))

(defn- populations []
  {:a (population :a [9000 8000 7000])
   :b (population :b [6000 5000 4000])})

(defn- options [mode]
  (cond-> {:collaboration-mode mode :final-evaluation? false}
    (= mode :oracle)
    (assoc :oracle-fitness-fns {:a (constantly 11) :b (constantly 22)}
           :oracle-reference-metadata {:a {:reference-id "new-a"}
                                       :b {:reference-id "new-b"}})))

(deftest supplied-populations-are-re-evaluated-in-every-mode
  (doseq [mode [:balanced :cartesian :panel :oracle]]
    (testing (name mode)
      (let [input (-> (populations)
                      (assoc-in [:a 0 :fitness-score] ##NaN)
                      (update-in [:b 1] dissoc :fitness-score))
            credit-inputs (atom [])
            result (nature/evolve-cooperatively
                    (species :a 3) (species :b 3) 0 (constantly 33)
                    (assoc (options mode) :initial-populations input
                           :credit-policy (fn [context]
                                            (swap! credit-inputs conj (:individual context))
                                            (:score (first (:encounters context))))))]
        (is (= 0 (:generation result)))
        (is (= 6 (count @credit-inputs)))
        (is (every? nil? (map :fitness-score @credit-inputs)))
        (doseq [id [:a :b]]
          (is (= (mapv #(dissoc % :fitness-score) (get input id))
                 (mapv #(dissoc % :fitness-score) (get-in result [:populations id]))))
          (is (= (repeat 3 (if (= mode :oracle) (if (= id :a) 11 22) 33))
                 (map :fitness-score (get-in result [:populations id])))))
        (is (= :skipped (:final-evaluation-status result)))
        (is (= [] (:final-collaborations result)))
        (is (s/valid? ::spec/coevolution-result result))))))

(deftest continued-oracle-run-selects-on-new-fitness-and-adds-generations
  (let [states (atom [])
        created (atom 0)
        config (fn [id]
                 (assoc (species id 3 #(do (swap! created inc) [id 10]))
                        :carry-over 1 :insert-new 2))
        input (populations)
        ;; The previous context favored member 3; the new context favors member 1.
        old-context (nature/evolve-cooperatively
                     (species :a 3) (species :b 3) 0 nil
                     (assoc (options :oracle) :initial-populations input
                            :oracle-fitness-fns {:a second :b second}))
        result (nature/evolve-cooperatively
                (config :a) (config :b) 2 nil
                (assoc (options :oracle) :initial-populations (:populations old-context)
                       :oracle-fitness-fns {:a #(- 100 (second %)) :b #(- 100 (second %))}
                       :monitors [#(swap! states conj %)]))]
    (is (= [0 1 2] (mapv :generation @states)))
    (is (= 2 (:generation result)))
    (is (= 8 @created))
    (doseq [id [:a :b]
            :let [elite (first (filter #(= (str (name id) "-1") (:guid %))
                                       (get-in result [:populations id])))]]
      (is (= (+ 2 (get-in input [id 0 :age])) (:age elite)))
      (is (= [id 1] (:genetic-sequence elite)))
      (is (= 99 (:fitness-score elite)))
      (is (= ["earlier-parent"] (:parents elite))))
    (doseq [state @states]
      (is (= {:a {:reference-id "new-a"} :b {:reference-id "new-b"}}
             (:oracle-reference-metadata state)))
      (is (= 6 (:oracle-evaluation-count state))))))

(deftest continued-panel-context-does-not-retain-old-history
  (let [old (nature/evolve-cooperatively
             (species :a 3) (species :b 3) 0 (constantly 9000)
             (assoc (options :panel) :initial-populations (populations)))
        new (nature/evolve-cooperatively
             (species :a 3) (species :b 3) 0 (constantly 7)
             (assoc (options :panel) :initial-populations (:populations old)))]
    (is (every? #(= 9000 (:metric-value %)) (mapcat vals (vals (:panel-history old)))))
    (is (every? #(= 7 (:metric-value %)) (mapcat vals (vals (:panel-history new)))))
    (is (every? #(zero? (:source-generation %)) (mapcat vals (vals (:panel-history new)))))
    (is (every? #(= [{:selector-name :bootstrap}] %)
                (mapcat vals (vals (:panel-provenance new)))))
    (is (nil? (:next-panels new)))))

(deftest continued-pair-modes-select-parents-only-after-fresh-scoring
  (doseq [mode [:balanced :cartesian :panel]]
    (let [config (fn [id]
                   (assoc (species id 3 (constantly [id 10]))
                          :carry-over 1 :insert-new 2))
          input (populations)
          result (nature/evolve-cooperatively
                  (config :a) (config :b) 1 (fn [[_ a] [_ b]] (+ a b))
                  (cond-> (assoc (options mode) :initial-populations input)
                    (= mode :balanced) (assoc :opponents 3)))]
      ;; Supplied scores favor member 1; the new additive objective favors 3.
      (doseq [id [:a :b]
              :let [surviving (filter #(= (str (name id) "-3") (:guid %))
                                      (get-in result [:populations id]))]]
        (is (= 1 (count surviving)))
        (is (= 6 (:age (first surviving))))
        (is (not-any? #(= (str (name id) "-1") (:guid %))
                      (get-in result [:populations id])))))))

(deftest supplied-unequal-populations-in-cartesian-and-oracle-modes
  (doseq [mode [:cartesian :oracle]]
    (let [input {:a (population :a [1 2 3]) :b (population :b [4 5])}
          result (nature/evolve-cooperatively
                  (species :a 3) (species :b 2) 0 (constantly 9)
                  (assoc (options mode) :initial-populations input))]
      (is (= [3 2] (mapv #(count (get-in result [:populations %])) [:a :b])))
      (is (s/valid? ::spec/coevolution-result result)))))

(deftest supplied-population-validation-precedes-initialization-and-evaluation
  (let [input (populations)
        bad-inputs [nil [] {} (dissoc input :b) (assoc input :extra [])
                    (update input :a pop) (assoc input :a #{})
                    (assoc-in input [:a 1] (get-in input [:a 0]))
                    (assoc-in input [:b 0 :guid] "")
                    (assoc-in input [:b 0 :genetic-sequence] [])
                    (assoc-in input [:b 0 :age] -1)
                    (assoc-in input [:b 0 :age] 0.5)
                    (update-in input [:b 0] dissoc :parents)]]
    (doseq [mode [:balanced :cartesian :panel :oracle] input bad-inputs]
      (let [calls (atom 0)
            generator #(do (swap! calls inc) [:new])
            scorer (fn [& _] (swap! calls inc) 1)]
        (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                     (nature/evolve-cooperatively
                      (species :a 3 generator) (species :b 3 generator) 1 scorer
                      (cond-> (assoc (options mode) :initial-populations input)
                        (= mode :oracle) (assoc :oracle-fitness-fns {:a scorer :b scorer})))))
        (is (zero? @calls))))))

(deftest refresh-retains-best-members-and-clears-all-fitness
  (let [created (atom [])
        config (fn [id n]
                 (species id n #(do (swap! created conj id) [id :new])))
        input {:a (population :a [5 9 9 1]) :b (population :b [1 8 3])}
        result (nature/refresh-cooperative-populations
                (config :a 4) (config :b 3) input {:a 2 :b 1})]
    (is (= {:a 2 :b 1} (frequencies @created)))
    (is (= ["a-2" "a-3"] (mapv :guid (take 2 (:a result)))))
    (is (= ["b-2" "b-3"] (mapv :guid (take 2 (:b result)))))
    (is (= {:a 4 :b 3} (into {} (map (fn [[id pop]] [id (count pop)]) result))))
    (doseq [id [:a :b]
            :let [old (into {} (map (juxt :guid identity) (get input id)))]
            individual (get result id)]
      (is (nil? (:fitness-score individual)))
      (if-let [survivor (get old (:guid individual))]
        (is (= (dissoc survivor :fitness-score) (dissoc individual :fitness-score)))
        (do (is (= 0 (:age individual)))
            (is (= pp/initializer-name (:parents individual)))
            (is (= [id :new] (:genetic-sequence individual))))))))

(deftest refresh-supports-zero-full-and-twenty-percent-replacement
  (doseq [n [0 20 100]]
    (let [created (atom 0)
          config (fn [id] (species id 100 #(do (swap! created inc) [id :new])))
          input {:a (population :a (range 100)) :b (population :b (range 100))}
          refreshed (nature/refresh-cooperative-populations
                     (config :a) (config :b) input {:a n :b n})]
      (is (= (* 2 n) @created))
      (doseq [id [:a :b]
              :let [old-ids (set (map :guid (get input id)))
                    kept (filter #(contains? old-ids (:guid %)) (get refreshed id))]]
        (is (= 100 (count (get refreshed id))))
        (is (= (- 100 n) (count kept)))
        (is (= 100 (count (set (map :guid (get refreshed id))))))
        (when (zero? n)
          (is (= (mapv :guid (get input id)) (mapv :guid (get refreshed id)))))))))

(deftest refresh-output-can-continue-through-a-new-oracle-context
  (let [input (populations)
        config (fn [id] (assoc (species id 3 (constantly [id 10])) :carry-over 3))
        refreshed (nature/refresh-cooperative-populations
                   (config :a) (config :b) input {:a 1 :b 1})
        states (atom [])
        result (nature/evolve-cooperatively
                (config :a) (config :b) 1 nil
                (assoc (options :oracle) :initial-populations refreshed
                       :oracle-fitness-fns {:a second :b second}
                       :monitors [#(swap! states conj %)]))]
    (is (= [0 1] (mapv :generation @states)))
    (doseq [id [:a :b]]
      (is (= (set (map :guid (get refreshed id)))
             (set (map :guid (get-in result [:populations id])))))
      (is (= #{1 2 10} (set (map :fitness-score (get-in result [:populations id]))))))))

(deftest refresh-validation-is-complete-before-generating-new-members
  (let [input (populations)
        invalid-counts [nil {} {:a 1} {:a 1 :b 0 :extra 0}
                        {:a 1 :b -1} {:a 1 :b 4} {:a 1 :b 0.5}]
        invalid-populations [(assoc-in input [:b 0 :fitness-score] nil)
                             (assoc-in input [:b 0 :fitness-score] ##NaN)
                             (assoc-in input [:b 0 :fitness-score] ##Inf)
                             (assoc-in input [:b 1] (get-in input [:b 0]))]]
    (doseq [[pop counts] (concat (map #(vector input %) invalid-counts)
                                (map #(vector % {:a 1 :b 1}) invalid-populations))]
      (let [calls (atom 0)
            generator #(do (swap! calls inc) [:new])]
        (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                     (nature/refresh-cooperative-populations
                      (species :a 3 generator) (species :b 3 generator) pop counts)))
        (is (zero? @calls))))
    (testing "a generated ID cannot reuse even a discarded individual's ID"
      (with-redefs [initialization/uuid (constantly "a-3")]
        (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                     (nature/refresh-cooperative-populations
                      (species :a 3 (constantly [:new])) (species :b 3)
                      input {:a 1 :b 0})))))))

(deftest final-cartesian-reporting-can-be-skipped-without-changing-evolution
  (doseq [mode [:balanced :cartesian :panel :oracle]]
    (let [calls (atom 0)
          opts (assoc (options mode) :initial-populations (populations)
                      :final-ratio 0.5
                      :final-evaluation-fn (fn [_ _] (swap! calls inc) :ordinary))
          skipped (nature/evolve-cooperatively
                   (species :a 3) (species :b 3) 0 (constantly 1) opts)
          performed (nature/evolve-cooperatively
                     (species :a 3) (species :b 3) 0 (constantly 1)
                     (dissoc opts :final-evaluation?))]
      (is (= 4 @calls))
      (is (= :skipped (:final-evaluation-status skipped)))
      (is (= [] (:final-collaborations skipped)))
      (is (= [2 2] (mapv #(count (get-in skipped [:solutions %])) [:a :b])))
      (is (= (:populations skipped) (:populations performed)))
      (is (= 4 (count (:final-collaborations performed))))
      (is (not (contains? performed :final-evaluation-status)))
      (is (s/valid? ::spec/coevolution-result skipped))
      (is (s/valid? ::spec/coevolution-result performed))
      (is (not (s/valid? ::spec/coevolution-result
                        (assoc performed :final-evaluation-status :skipped)))))))

(deftest invalid-final-reporting-option-fails-before-initialization
  (doseq [flag [nil 0 "false"]]
    (let [calls (atom 0)
          generator #(do (swap! calls inc) [:new])]
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                   (nature/evolve-cooperatively
                    (species :a 1 generator) (species :b 1 generator) 0 (constantly 1)
                    {:final-evaluation? flag})))
      (is (zero? @calls)))))
