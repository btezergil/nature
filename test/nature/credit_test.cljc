(ns nature.credit-test
  (:require [nature.credit :as credit]
            [nature.core :as nature]
            [nature.panel :as panel]
            [nature.panel-selectors :as selectors]
            [nature.initialization-operators :as io]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(defn context [values] {:encounters (mapv #(hash-map :score %) values)})
(defn error? [f]
  (try (f) false (catch #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo) _ true)))

(deftest scalar-policies-test
  (let [specialist (context [0 10 0]) generalist (context [6 6 6])]
    (is (< (credit/mean specialist) (credit/mean generalist)))
    (is (> (credit/maximum specialist) (credit/maximum generalist)))
    (is (= 5 (credit/top-two-mean specialist)))
    (is (= 7 ((credit/weighted [7 2 1]) specialist)))
    (is (= 7 ((credit/weighted [7 2 1]) (context [0 0 10])))))
  (doseq [f [credit/mean credit/maximum credit/top-two-mean (credit/weighted [3 2 1])]]
    (is (= -4 (f (context [-4]))))
    (doseq [values [[] [##NaN] [##Inf] [nil] ["1"]]]
      (is (error? #(f (context values))))))
  (is (= 8 ((credit/weighted [3 1 100]) (context [2 10]))))
  (is (error? #((credit/weighted [1]) (context [1 2]))))
  (doseq [weights [nil [] [0 1] [1 -1] [##Inf] [##NaN] ["1"] #{1}]]
    (is (error? #(credit/weighted weights))))
  (doseq [options [{:panel-credit nil} {:panel-credit :unknown}
                   {:panel-credit :weighted} {:panel-credit-weights [1]}]]
    (is (error? #(credit/resolve-options options)))))

(defn individual [guid n] {:guid guid :genetic-sequence [n] :fitness-score nil})
(def pops {:a [(individual "a1" 1) (individual "a2" 2)]
           :b [(individual "b1" 1) (individual "b2" 2)]})
(defn fitness [[a] [b]] (if (= a 1) 6 (if (= b 1) 10 0)))

(deftest raw-evidence-and-callback-context-test
  (let [seen (atom [])
        state (panel/evaluate :a :b pops pops fitness
                              (fn [ctx] (swap! seen conj ctx) (credit/maximum ctx)) 7)
        history (panel/update-history {} (assoc state :generation 7 :panels pops))]
    (is (= [6 10] (mapv :fitness-score (get-in state [:populations :a]))))
    (is (= [6 5] (mapv #(get-in state [:panel-statistics :a % :average-score]) ["a1" "a2"])))
    (is (= "a2" (get-in history [:a :fitness :individual :guid])))
    (is (= "a1" (get-in history [:a :average :individual :guid])))
    (is (= 4 (count @seen)))
    (doseq [{:keys [generation species-id individual collaborator-species-id panel encounters]} @seen]
      (is (= 7 generation))
      (is (not= species-id collaborator-species-id))
      (is (= (get pops collaborator-species-id) panel))
      (is (= (mapv :guid panel) (mapv :collaborator-guid encounters)))
      (is (every? #(and (= species-id (:focal-species-id %))
                        (= (:guid individual) (:focal-guid %))) encounters)))
    (doseq [value [nil ##NaN ##Inf "1"]]
      (is (error? #(panel/evaluate :a :b pops pops fitness (constantly value) 7))))))

(defn species [id]
  (let [counter (atom 0)]
    {:species-id id :population-size 2 :genome-generator #(vector (swap! counter inc))
     :carry-over 1 :insert-new 0 :binary-operators [(fn [a b] [a b])]}))

(defn deterministic-run [options]
  (let [ids (atom 0) draws (atom 0) states (atom [])]
    (with-redefs [io/uuid #(str (swap! ids inc))
                  #?(:clj clojure.core/rand :cljs cljs.core/rand) (fn ([] (swap! draws inc) 0.75)
                                     ([n] (swap! draws inc) (* 0.75 n)))
                  #?(:clj clojure.core/rand-nth :cljs cljs.core/rand-nth) first]
      (let [result (nature/evolve-cooperatively
                    (species :a) (species :b) 2 fitness
                    (merge {:collaboration-mode :panel
                            :panel-selection-fns [selectors/all-members]
                            :monitors [#(swap! states conj %)]
                            :final-evaluation-fn (constantly :final)} options))]
        {:result result :states @states :draws @draws}))))

(deftest lifecycle-credit-and-default-regression-test
  (let [implicit (deterministic-run {}) explicit (deterministic-run {:panel-credit :mean})
        maximum (deterministic-run {:panel-credit :maximum})
        state (second (:states maximum)) terminal (last (:states maximum))]
    (is (= implicit explicit))
    (is (pos? (:draws implicit)))
    (is (= :maximum (:panel-credit-policy terminal)))
    (is (= 10 (get-in state [:panel-history :a :fitness :metric-value])))
    (is (= 10 (get-in state [:panel-history :a :average :metric-value])))
    (is (= (->> (get-in state [:populations :a]) (sort-by :fitness-score >) first :guid)
           (get-in terminal [:populations :a 0 :guid])))
    (is (every? #(= :final (:result %)) (get-in maximum [:result :final-collaborations])))))

(deftest invalid-configuration-precedes-initialization-test
  (let [generated (atom 0) a (assoc (species :a) :genome-generator #(do (swap! generated inc) [1]))]
    (doseq [opts [{:collaboration-mode :balanced :panel-credit :mean}
                  {:collaboration-mode :cartesian :panel-credit-weights [1]}
                  {:collaboration-mode :panel :panel-credit :weighted :panel-credit-weights [0 1]}
                  {:collaboration-mode :panel :panel-credit :bad}]]
      (is (error? #(nature/evolve-cooperatively a (species :b) 1 fitness opts))))
    (is (zero? @generated))))

(deftest custom-credit-drives-parent-sampling-and-selectors-test
  (let [seen (atom [])
        policy (fn [{:keys [individual]}]
                 (if (= [2] (:genetic-sequence individual)) 9 1))
        {:keys [states]} (deterministic-run
                         {:panel-credit policy
                          :panel-selection-fns
                          [(fn [ctx] (swap! seen conj ctx) (selectors/best-fitness ctx))]})
        initial (first states) next-state (second states)]
    (is (= :custom (:panel-credit-policy initial)))
    (doseq [id [:a :b]
            :let [winner (second (get-in initial [:populations id]))
                  child (second (get-in next-state [:populations id]))]]
      ;; With target .75 and credits [1,9], both weighted draws select winner.
      (is (= [(:guid winner) (:guid winner)] (:parents child)))
      (is (= (:guid winner) (get-in next-state [:populations id 0 :guid])))
      (is (= 9 (get-in initial [:panel-history id :fitness :metric-value])))
      (is (= [(:guid winner)] (mapv :guid (get-in initial [:next-panels id])))))
    (is (= 4 (count @seen)))))
