(ns nature.panel-test
  (:require [nature.panel :as panel]
            [nature.panel-selectors :as selectors]
            [nature.core :as nature]
            [nature.spec :as spec]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(defn individual [guid n]
  {:guid guid :genetic-sequence [n] :fitness-score nil :age 0 :parents [:initial]})

(def a [(individual "a1" 1) (individual "a2" 2)])
(def b [(individual "b1" 10) (individual "b2" 20) (individual "b3" 30)])
(defn pair-score [[x] [y]] (+ (* 100 x) y))
(defn error? [f]
  (try (f) false (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) _ true)))

(deftest directional-evaluation-test
  (let [calls (atom [])
        panels {:a [(first a)] :b [(first b) (second b)]}
        state (panel/evaluate :a :b {:a a :b b} panels
                              (fn [x y] (swap! calls conj [x y]) (pair-score x y)))]
    (is (= 7 (:directional-collaboration-count state)))
    (is (= 5 (:unique-collaboration-evaluation-count state) (count @calls)))
    (is (= 5 (count (set @calls))))
    (is (= [115 215] (mapv :fitness-score (get-in state [:populations :a]))))
    (is (= [110 120 130] (mapv :fitness-score (get-in state [:populations :b]))))
    (doseq [[id population other-id] [[:a a :b] [:b b :a]]
            focal population]
      (let [records (filter #(and (= id (:focal-species-id %))
                                  (= (:guid focal) (:focal-guid %))) (:collaborations state))]
        (is (= (set (map :guid (get panels other-id)))
               (set (map :collaborator-guid records))))
        (is (= (count (get panels other-id)) (count records)))))
    (doseq [[_ records] (group-by :participants (:collaborations state))]
      (is (apply = (map :score records))))
    (is (= {:fitness-score 115 :average-score 115 :maximum-score 120 :encounter-count 2}
           (get-in state [:panel-statistics :a "a1"])))))

(deftest archived-and-species-aware-evaluation-test
  (let [archived (individual "old" 8)
        state (panel/evaluate :a :b {:a a :b b}
                              {:a [archived] :b [(first b)]} pair-score)]
    (is (= 5 (:unique-collaboration-evaluation-count state))))
  (let [state (panel/evaluate :a :b {:a [(individual "same" 1)]
                                    :b [(individual "same" 10)]}
                              {:a [(individual "archived" 2)]
                               :b [(individual "archived" 20)]} pair-score)]
    (is (= 120 (get-in state [:populations :a 0 :fitness-score])))
    (is (= 210 (get-in state [:populations :b 0 :fitness-score])))))

(deftest panel-validation-test
  (doseq [functions [[] nil #{} [nil] [(constantly []) 3]]]
    (is (error? #(panel/validate-selectors functions))))
  (let [ctx {:generation 1 :source-generation 0 :species-id :a :population a
             :history {} :previous-panel []}]
    (doseq [selector [(constantly nil) (constantly #{}) (constantly [])
                      (constantly [(individual "unknown" 3)])
                      (fn [{:keys [population]}] [(assoc (first population) :age 99)])]]
      (is (error? #(panel/assemble [selector] ctx))))
    (let [seen (atom [])
          output (panel/assemble
                  [(constantly []) selectors/all-members
                   (fn [context] (swap! seen conj context) [(first a)])] ctx)]
      (is (= a (:panel output)))
      (is (= a (:selected-panel (first @seen))))
      (is (= [1 2] (mapv :selector-index (get-in output [:provenance "a1"]))))
      (is (= 1 (count (get-in output [:provenance "a2"])))))
    (try
      (panel/assemble [(fn [_] (throw (ex-info "consumer error" {})))] ctx)
      (is false "Expected selector error")
      (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) e
        (is (= {:generation 1 :source-generation 0 :species-id :a :selector-index 0}
               (select-keys (ex-data e) [:generation :source-generation :species-id :selector-index]))))))
  (doseq [[populations panels]
          [[{:a [(first a) (first a)] :b b} {:a [(first a)] :b [(first b)]}]
           [{:a a :b b} {:a [] :b [(first b)]}]
           [{:a a :b b} {:a [(first a) (first a)] :b [(first b)]}]
           [{:a a :b b} {:a [(assoc (first a) :genetic-sequence [999])] :b [(first b)]}]
           [{:a [(dissoc (first a) :genetic-sequence)] :b b} {:a [(first a)] :b [(first b)]}]
           [{:a a :b b} {:a [{:guid "bad" :genetic-sequence []}] :b [(first b)]}]
           [{:a a :b b} {:a [(first a)]}]]]
    (is (error? #(panel/evaluate :a :b populations panels pair-score))))
  (doseq [score [nil "1" ##NaN ##Inf ##-Inf]]
    (is (error? #(panel/evaluate :a :b {:a a :b b}
                                {:a [(first a)] :b [(first b)]} (constantly score))))))

(defn run-species [id size]
  (let [counter (atom 0)]
    {:species-id id :population-size size
     :genome-generator #(vector id (swap! counter inc))
     :binary-operators [] :unary-operators []
     :carry-over 0 :insert-new size}))

(deftest panel-lifecycle-history-and-monitor-test
  (let [states (atom []) contexts (atom []) calls (atom 0)
        selector (fn [ctx]
                   (swap! contexts conj ctx)
                   (concat (selectors/best-fitness ctx)
                           (selectors/historical-best-fitness ctx)))
        result (nature/evolve-cooperatively
                (run-species :a 2) (run-species :b 3) 2
                (fn [[id-a x] [id-b y]]
                  (is (= [:a :b] [id-a id-b]))
                  (swap! calls inc)
                  (- (+ x y)))
                {:collaboration-mode :panel :panel-selection-fns [selector]
                 :monitors [#(swap! states conj %)] :final-ratio 0.5
                 :final-evaluation-fn (fn [[id-a _] [id-b _]] [id-a id-b])})
        [initial middle terminal] @states]
    (is (= [0 1 2] (mapv :generation @states)))
    (is (= 4 (count @contexts)))
    (is (= #{[1 0 :a] [1 0 :b] [2 1 :a] [2 1 :b]}
           (set (map (juxt :generation :source-generation :species-id) @contexts))))
    (doseq [[before after] (partition 2 1 @states)]
      (is (= (:next-panels before) (:panels after)))
      (is (= (:next-panel-provenance before) (:panel-provenance after))))
    (doseq [state @states]
      (is (s/valid? ::spec/coevolution-state state))
      (doseq [id [:a :b]]
        (is (= #{:fitness :average :maximum} (set (keys (get-in state [:panel-history id])))))))
    (is (= @calls (reduce + (map :unique-collaboration-evaluation-count @states))))
    (is (= 4 (:unique-collaboration-evaluation-count initial)))
    (is (= 5 (:unique-collaboration-evaluation-count middle)))
    (is (nil? (:next-panels terminal)))
    (is (= terminal (dissoc result :solutions :final-collaborations)))
    (is (s/valid? ::spec/coevolution-result result))
    (is (not (s/valid? ::spec/coevolution-result (dissoc result :panel-history))))
    (is (not (s/valid? ::spec/coevolution-result
                       (assoc result :directional-collaboration-count 999))))
    (is (= 2 (count (:final-collaborations result))))
    (is (every? #(= [:a :b] (:result %)) (:final-collaborations result)))
    (doseq [id [:a :b]]
      (is (= 0 (get-in terminal [:panel-history id :fitness :source-generation])))
      (is (= (get-in initial [:panel-history id]) (get-in terminal [:panel-history id]))))
    (is (not-any? (set (map :guid (get-in terminal [:populations :a])))
                  (map :guid (get-in terminal [:panels :a]))))))

(deftest terminal-history-bootstrap-and-default-test
  (let [states (atom [])
        result (nature/evolve-cooperatively
                (run-species :a 2) (run-species :b 2) 2
                (fn [[_ x] [_ y]] (+ x y))
                {:collaboration-mode :panel
                 :monitors [#(swap! states conj %)]
                 :final-evaluation-fn (constantly :done)})]
    (is (= 2 (get-in result [:panel-history :a :fitness :source-generation])))
    (is (= 2 (get-in result [:panel-history :b :maximum :source-generation])))
    (is (= :bootstrap (-> @states first :panel-provenance :a vals first first :selector-name)))
    (is (= :random (-> @states second :panel-provenance :a vals first first :selector-name))))
  (let [result (nature/evolve-cooperatively
                (run-species :a 1) (run-species :b 1) 0 (constantly -3)
                {:collaboration-mode :panel
                 :panel-selection-fns [(fn [_] (throw (ex-info "must not run" {})))]})]
    (is (= -3 (get-in result [:panel-history :a :fitness :metric-value])))
    (is (= 0 (get-in result [:panel-history :a :fitness :source-generation])))
    (is (nil? (:next-panels result)))
    (is (s/valid? ::spec/coevolution-result result)))
  (let [result (nature/evolve-cooperatively
                (run-species :a 1) (run-species :b 1) 2 (constantly -3)
                {:collaboration-mode :panel})]
    (is (= 0 (get-in result [:panel-history :a :fitness :source-generation]))))
  (let [generated (atom 0)
        species (assoc (run-species :a 1) :genome-generator #(do (swap! generated inc) [1]))]
    (doseq [invalid [[] nil [42]]]
      (is (error? #(nature/evolve-cooperatively species (run-species :b 1) 0 (constantly 1)
                                              {:collaboration-mode :panel :panel-selection-fns invalid}))))
    (is (zero? @generated))))

(deftest complete-selector-bundle-test
  (let [states (atom [])
        distance (fn [[_ x] [_ y] _] (#?(:clj Math/abs :cljs js/Math.abs) (- x y)))
        selection-fns [selectors/best-fitness (selectors/random-members 1)
                       selectors/specialist selectors/generalist
                       (selectors/diverse-strong distance)
                       selectors/historical-best-fitness
                       selectors/historical-best-average
                       selectors/historical-best-maximum]
        result (nature/evolve-cooperatively
                (run-species :a 12) (run-species :b 9) 2
                (fn [[_ x] [_ y]] (- 100 (#?(:clj Math/abs :cljs js/Math.abs) (- x y))))
                {:collaboration-mode :panel :panel-selection-fns selection-fns
                 :monitors [#(swap! states conj %)]
                 :final-evaluation-fn (constantly :done)})]
    (is (s/valid? ::spec/coevolution-result result))
    (doseq [state (rest @states)
            id [:a :b]
            :let [members (get-in state [:panels id])
                  origins (mapcat val (get-in state [:panel-provenance id]))]]
      (is (= (count members) (count (set (map :guid members)))))
      (is (every? (set (map :selector-name origins))
                  [:best-fitness :random :specialist :generalist
                   :historical-best-fitness :historical-best-average :historical-best-maximum])))
    (doseq [state @states]
      (is (s/valid? ::spec/coevolution-state state)))))

(deftest same-pair-is-reevaluated-next-generation-test
  (let [calls (atom 0) states (atom [])
        species (fn [id] (assoc (run-species id 1) :carry-over 1 :insert-new 0))]
    (nature/evolve-cooperatively
     (species :a) (species :b) 2
     (fn [_ _] (swap! calls inc))
     {:collaboration-mode :panel :panel-selection-fns [selectors/best-fitness]
      :monitors [#(swap! states conj %)]
      :final-evaluation-fn (constantly :done)})
    (is (= 3 @calls))
    (is (= [1 2 3] (mapv #(get-in % [:populations :a 0 :fitness-score]) @states)))
    (is (apply = (map #(get-in % [:populations :a 0 :guid]) @states)))))

(deftest snapshot-deduplication-and-history-metrics-test
  (let [old (assoc (first a) :fitness-score 10)
        current (assoc old :age 1 :fitness-score 20)
        ctx {:species-id :a :population [current]
             :history {:fitness {:individual old}} :previous-panel []}
        assembled (panel/assemble [selectors/historical-best-fitness selectors/best-fitness] ctx)]
    (is (= [old] (:panel assembled)))
    (is (= 2 (count (get-in assembled [:provenance "a1"]))))
    (is (error? #(panel/assemble
                  [selectors/historical-best-fitness]
                  (assoc ctx :population [(assoc current :genetic-sequence [999])])))))
  (let [state (merge {:generation 0 :panels {:a a :b [(first b) (second b)]}}
                     (panel/evaluate :a :b {:a a :b b}
                                     {:a a :b [(first b) (second b)]}
                                     (fn [[x] [y]]
                                       (if (= x 1) 6 (if (= y 10) 10 0)))))
        history (panel/update-history {} state)]
    (is (= "a1" (get-in history [:a :fitness :individual :guid])))
    (is (= "a1" (get-in history [:a :average :individual :guid])))
    (is (= "a2" (get-in history [:a :maximum :individual :guid])))
    (is (= 10 (get-in history [:a :maximum :metric-value])))))

(deftest legacy-modes-ignore-panel-options-test
  (doseq [mode [:balanced :cartesian]]
    (is (s/valid? ::spec/coevolution-result
                  (nature/evolve-cooperatively
                   (run-species :a 2) (run-species :b 2) 0 (constantly 1)
                   {:collaboration-mode mode :panel-selection-fns []})))))

(deftest historical-snapshots-and-ties-test
  (let [state (merge {:generation 0 :panels {:a [(first a)] :b [(first b)]}}
                     (panel/evaluate :a :b {:a a :b b}
                                     {:a [(first a)] :b [(first b)]} (constantly -1)))
        history (panel/update-history {} state)
        ctx {:species-id :a :population [(individual "new" 99)]
             :history (:a history) :previous-panel []}]
    (is (= (get-in state [:populations :a 0]) (get-in history [:a :fitness :individual])))
    (is (= history (panel/update-history history (assoc state :generation 1))))
    (is (= [(get-in history [:a :fitness :individual])]
           (:panel (panel/assemble [selectors/historical-best-fitness] ctx))))
    (is (= ["b1"] (get-in history [:a :maximum :opposite-panel-guids])))))

(deftest panel-reproduction-remains-species-local-test
  (let [states (atom [])
        species (fn [id]
                  (assoc (run-species id 3) :carry-over 1 :insert-new 0
                         :binary-operators [(fn [[id-a _ :as x] [id-b _ :as y]]
                                              (is (= id id-a id-b))
                                              [x y])]
                         :unary-operators [(fn [[genome-id _ :as x]] (is (= id genome-id)) x)]))]
    (nature/evolve-cooperatively
     (species :a) (species :b) 2 (constantly 1)
     {:collaboration-mode :panel :panel-selection-fns [selectors/all-members]
      :monitors [#(swap! states conj %)] :final-evaluation-fn (constantly nil)})
    (doseq [[previous current] (partition 2 1 @states)
            id [:a :b]
            :let [parents (set (map :guid (get-in previous [:populations id])))]
            child (get-in current [:populations id])
            :when (not (parents (:guid child)))]
      (is (every? parents (:parents child))))))

#?(:clj
   (deftest panel-evaluation-parallel-test
     (let [active (atom 0) peak (atom 0)]
       (panel/evaluate :a :b {:a a :b b} {:a a :b b}
                       (fn [_ _]
                         (swap! peak max (swap! active inc))
                         (Thread/sleep 30)
                         (swap! active dec)
                         1))
       (is (> @peak 1)))))
