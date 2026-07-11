(ns nature.core-test
  (:require [clojure.spec.alpha :as csa]
            [nature.core :as nature]
            [nature.spec :as s]
            [nature.genetic-operators :as go]
            [nature.population-presets :as pp]
            #? (:clj  [clojure.test :refer [deftest is testing]])
            #? (:cljs [cljs.test    :refer-macros [deftest is testing]])))

(deftest evolve-test
  (testing "Check that evolution is successful"
    (let [result (nature/evolve pp/binary-genome
                                pp/default-sequence-length
                                pp/default-population-size
                                pp/default-generation-count
                                pp/sum-alleles
                                [(go/crossover pp/sum-alleles)]
                                [(partial go/mutation-operator pp/sum-alleles pp/binary-genome 1)])]
      (is (csa/valid? ::s/population result))
      (is (= 1 (count result))))))

(deftest evolve-with-sequence-generator-initial-retry-test
  (testing "Keeps default behavior when initial population retries are not configured"
    (let [result (nature/evolve-with-sequence-generator
                  (constantly [0])
                  1
                  0
                  first
                  []
                  [])]
      (is (= 0 (:fitness-score (first result))))))

  (testing "Retries the initial population when configured and all fitness scores are zero"
    (let [generated (atom 0)
          result (nature/evolve-with-sequence-generator
                  #(vector (swap! generated inc))
                  1
                  0
                  #(if (= 1 (first %)) 0 1)
                  []
                  []
                  {:initial-population-retries 1})]
      (is (= 1 (:fitness-score (first result))))))

  (testing "Throws when configured retries cannot produce a non-zero initial population"
    (is (thrown-with-msg?
         #?(:clj clojure.lang.ExceptionInfo
            :cljs cljs.core/ExceptionInfo)
         #"Initial population has all-zero fitness"
         (nature/evolve-with-sequence-generator
          (constantly [0])
          1
          0
          first
          []
          []
          {:initial-population-retries 1})))))
