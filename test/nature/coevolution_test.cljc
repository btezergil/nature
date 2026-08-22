(ns nature.coevolution-test
  (:require [clojure.spec.alpha :as csa]
            [nature.core :as nature]
            [nature.population-presets :as pp]
            [nature.spec :as s]
            #? (:clj  [clojure.test :refer [deftest is testing]])
            #? (:cljs [cljs.test :refer-macros [deftest is testing]])))

(defn- numbered-generator
  [prefix counter]
  #(vector prefix (swap! counter inc)))

(defn- species
  ([species-id population-size generator]
   (species species-id population-size generator {}))
  ([species-id population-size generator overrides]
   (merge {:species-id species-id
           :population-size population-size
           :genome-generator generator
           :binary-operators []
           :unary-operators []
           :carry-over 0
           :insert-new 0}
          overrides)))

(defn- mean
  [xs]
  (/ (reduce + xs) (count xs)))

(deftest balanced-collaboration-scheduling-and-credit-test
  (let [counter-a (atom 0)
        counter-b (atom 0)
        result (nature/evolve-cooperatively
                (species :a 4 (numbered-generator :a counter-a))
                (species :b 4 (numbered-generator :b counter-b))
                0
                (fn [[_ a] [_ b]] (+ (* 10 a) b))
                {:collaboration-mode :balanced
                 :opponents 2})
        collaborations (:collaborations result)]
    (testing "population sizes and result spec"
      (is (= 4 (count (get-in result [:populations :a]))))
      (is (= 4 (count (get-in result [:populations :b]))))
      (is (csa/valid? ::s/coevolution-result result)))
    (testing "every member has exactly K unique opponents and every pair is evaluated once"
      (is (= 8 (count collaborations)))
      (is (= 8 (count (distinct (map :participants collaborations)))))
      (doseq [species-id [:a :b]
              individual (get-in result [:populations species-id])]
        (let [guid (:guid individual)
              credited (filter #(= guid (get-in % [:participants species-id]))
                               collaborations)
              other-id (if (= :a species-id) :b :a)]
          (is (= 2 (count credited)))
          (is (= 2 (count (distinct (map #(get-in % [:participants other-id]) credited))))))))
    (testing "the same collaboration score is averaged and credited to both participants"
      (doseq [species-id [:a :b]
              individual (get-in result [:populations species-id])]
        (let [scores (map :score
                          (filter #(= (:guid individual)
                                      (get-in % [:participants species-id]))
                                  collaborations))]
          (is (= (mean scores) (:fitness-score individual))))))))

(deftest balanced-collaboration-validation-test
  (testing "K cannot exceed the population size"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"cannot exceed"
         (nature/evolve-cooperatively
          (species :a 2 (constantly [:a]))
          (species :b 2 (constantly [:b]))
          0 (constantly 1) {:opponents 3}))))
  (testing "equal sizes are necessary for exactly K opponents on both sides"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
         #"equally sized"
         (nature/evolve-cooperatively
          (species :a 2 (constantly [:a]))
          (species :b 3 (constantly [:b]))
          0 (constantly 1) {:opponents 1})))))

#?(:clj
   (deftest collaboration-evaluation-is-parallel-test
     (let [active-calls (atom 0)
           maximum-active-calls (atom 0)
           result (nature/evolve-cooperatively
                   (species :a 4 (constantly [:a]))
                   (species :b 4 (constantly [:b]))
                   0
                   (fn [_genome-a _genome-b]
                     (let [active (swap! active-calls inc)]
                       (swap! maximum-active-calls max active)
                       (Thread/sleep 50)
                       (swap! active-calls dec)
                       1))
                   {:opponents 1
                    :final-ratio 0.25
                    :final-evaluation-fn (constantly :final)})]
       (is (= 4 (count (:collaborations result))))
       (is (> @maximum-active-calls 1)))))

(deftest independent-reproduction-and-re-evaluation-test
  (let [counter-a (atom 0)
        counter-b (atom 0)
        binary-calls-a (atom [])
        binary-calls-b (atom [])
        unary-calls-a (atom [])
        unary-calls-b (atom [])
        fitness-calls (atom [])
        states (atom [])
        binary-a (fn [left right]
                   (swap! binary-calls-a conj [left right])
                   [[:a-child left right]])
        binary-b (fn [left right]
                   (swap! binary-calls-b conj [left right])
                   [[:b-child left right]])
        unary-a (fn [genome]
                  (swap! unary-calls-a conj genome)
                  (conj genome :a-mutated))
        unary-b (fn [genome]
                  (swap! unary-calls-b conj genome)
                  (conj genome :b-mutated))
        result (nature/evolve-cooperatively
                (species :a 3 (numbered-generator :a counter-a)
                         {:binary-operators [binary-a]
                          :unary-operators [unary-a]
                          :carry-over 1
                          :insert-new 1})
                (species :b 3 (numbered-generator :b counter-b)
                         {:binary-operators [binary-b]
                          :unary-operators [unary-b]
                          :carry-over 1
                          :insert-new 1})
                1
                (fn [genome-a genome-b]
                  (swap! fitness-calls conj [genome-a genome-b])
                  1)
                {:opponents 1
                 :final-ratio (/ 1 3)
                 :final-evaluation-fn (constantly :final)
                 :monitors [#(swap! states conj %)]})
        initial (first @states)
        final (second @states)]
    (testing "selection and reproduction happen independently within each species"
      (is (= 1 (count @binary-calls-a)))
      (is (= 1 (count @binary-calls-b)))
      (is (every? #(= :a (first %)) (mapcat identity @binary-calls-a)))
      (is (every? #(= :b (first %)) (mapcat identity @binary-calls-b)))
      (is (= 1 (count @unary-calls-a)))
      (is (= 1 (count @unary-calls-b)))
      (doseq [species-id [:a :b]]
        (let [old-guids (set (map :guid (get-in initial [:populations species-id])))
              child (first (filter #(not= pp/initializer-name (:parents %))
                                   (get-in final [:populations species-id])))]
          (is (= 2 (count (:parents child))))
          (is (every? old-guids (:parents child))))))
    (testing "elites and newly inserted members are both evaluated in the new context"
      (is (= 6 (count @fitness-calls)))
      (doseq [species-id [:a :b]]
        (let [population (get-in final [:populations species-id])
              elite (first (filter #(= 1 (:age %)) population))
              inserted (first (filter #(and (= pp/initializer-name (:parents %))
                                            (zero? (:age %)))
                                      population))
              argument-index (if (= species-id :a) 0 1)
              evaluated-genomes (map #(nth % argument-index) @fitness-calls)]
          (is (= 2 (count (filter #(= (:genetic-sequence elite) %) evaluated-genomes))))
          (is (some #(= (:genetic-sequence inserted) %) evaluated-genomes))
          (is (every? number? (map :fitness-score population))))))
    (testing "final population sizes are preserved"
      (is (= 3 (count (get-in result [:populations :a]))))
      (is (= 3 (count (get-in result [:populations :b])))))))

(deftest cartesian-final-evaluation-and-monitor-test
  (let [counter-a (atom 0)
        counter-b (atom 0)
        final-calls (atom [])
        monitor-states (atom [])
        result (nature/evolve-cooperatively
                (species :a 3 (numbered-generator :a counter-a))
                (species :b 2 #(vector (* 10 (swap! counter-b inc))))
                0
                (fn [[_ a] [b]] (+ (* 100 a) b))
                {:collaboration-mode :cartesian
                 :final-ratio 0.5
                 :final-evaluation-fn
                 (fn [genome-a genome-b]
                   (swap! final-calls conj [genome-a genome-b])
                   {:pair [genome-a genome-b]})
                 :monitors [#(swap! monitor-states conj %)]})]
    (testing "Cartesian evaluation covers unequal populations"
      (is (= 6 (count (:collaborations result))))
      (is (= 3 (count (get-in result [:populations :a]))))
      (is (= 2 (count (get-in result [:populations :b])))))
    (testing "ceil top-ratio selections are ranked by contextual fitness"
      (is (= 2 (count (get-in result [:solutions :a]))))
      (is (= 1 (count (get-in result [:solutions :b]))))
      (is (= [[:a 3] [:a 2]]
             (mapv :genetic-sequence (get-in result [:solutions :a]))))
      (is (= [[20]]
             (mapv :genetic-sequence (get-in result [:solutions :b])))))
    (testing "the final evaluator is called for every selected Cartesian pair"
      (is (= 2 (count @final-calls)))
      (is (= 2 (count (:final-collaborations result))))
      (is (= (mapv (fn [[a b]] {:pair [a b]}) @final-calls)
             (mapv :result (:final-collaborations result)))))
    (testing "monitors receive both populations and every generation collaboration"
      (is (= 1 (count @monitor-states)))
      (is (csa/valid? ::s/coevolution-state (first @monitor-states)))
      (is (= #{:a :b} (set (keys (:populations (first @monitor-states))))))
      (is (= 6 (count (:collaborations (first @monitor-states))))))))
