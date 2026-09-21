(ns nature.panel-selectors-test
  (:require [nature.panel-selectors :as p]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])))

(defn individual [n fitness]
  {:guid (str n) :genetic-sequence [n] :fitness-score fitness})

(def candidates [(individual 0 10) (individual 1 9) (individual 2 8)])
(def context
  {:population candidates
   :statistics {"0" {:average-score 10 :maximum-score 11}
                "1" {:average-score 9 :maximum-score 100}
                "2" {:average-score 8 :maximum-score 12}}})

(deftest fitness-specialist-generalist-test
  (is (= [(first candidates)] (p/best-fitness context)))
  (is (= [(second candidates)] (p/specialist context)))
  (is (= [(first candidates)] (p/generalist context)))
  (is (= candidates (p/all-members context)))
  (is (= [(last candidates)]
         ((p/ranked-members 1 (fn [individual ctx]
                                (is (= context ctx))
                                (- (:fitness-score individual)))) context)))
  (is (= (take 2 candidates)
         ((p/ranked-members 2 (fn [_ _] 1)) context))))

(deftest random-and-count-test
  (let [selected ((p/random-members 2) context)]
    (is (= 2 (count selected)))
    (is (= 2 (count (set selected))))
    (is (every? (set candidates) selected)))
  (is (= (set candidates) (set ((p/random-members 10) context))))
  (is (= candidates ((p/ranked-members 10 (fn [x _] (:fitness-score x))) context)))
  (doseq [n [0 -1 1.5 nil]]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                 (p/random-members n)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                 (p/ranked-members n (fn [_ _] 1))))))

(deftest historical-selector-test
  (let [history {:fitness {:individual (first candidates)}
                 :average {:individual (second candidates)}
                 :maximum {:individual (last candidates)}}]
    (doseq [[selector expected] [[p/historical-best-fitness (first candidates)]
                                [p/historical-best-average (second candidates)]
                                [p/historical-best-maximum (last candidates)]]]
      (is (= [expected] (selector {:history history})))
      (is (= [] (selector {:history {}}))))))

(defn distance [[a] [b] _] (#?(:clj Math/abs :cljs js/Math.abs) (- a b)))

(deftest diverse-strong-test
  (let [population (mapv #(individual % (- 20 %)) (range 9))
        selector (p/diverse-strong distance)
        ctx {:population population}]
    (testing "ceil(9/4) gives three candidates, distance measured from the best"
      (is (= [(nth population 2)] (selector ctx))))
    (testing "maximin uses all earlier selections"
      (is (= [(second population)]
             (selector (assoc ctx :selected-panel [(first population) (nth population 2)])))))
    (testing "all top-quartile members already selected"
      (is (= [] (selector (assoc ctx :selected-panel (subvec population 0 3))))))
    (testing "equivalent genomes and singleton quartiles produce no replacement"
      (is (= [] ((p/diverse-strong (fn [_ _ _] 0)) ctx)))
      (is (= [] (selector {:population [(first population)]}))))
    (testing "fitness and stable population order break distance ties"
      (is (= [(first population)] ((p/diverse-strong (fn [_ _ _] 1)) ctx)))
      (let [tied (mapv #(assoc % :fitness-score 1) population)]
        (is (= [(first tied)]
               ((p/diverse-strong (fn [_ _ _] 1)) {:population tied})))))
    (testing "quartile boundary ties do not enlarge the candidate pool"
      (let [tied (mapv #(assoc % :fitness-score 1) population)]
        (is (= [(nth tied 2)] (selector {:population tied})))))
    (doseq [bad [-1 ##NaN ##Inf nil]]
      (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                   ((p/diverse-strong (fn [_ _ _] bad)) ctx))))))

(deftest invalid-ranking-test
  (doseq [bad [nil ##NaN ##Inf ##-Inf]]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
                 ((p/ranked-members 1 (fn [_ _] bad)) context))))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
               (p/ranked-members 1 nil)))
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs cljs.core/ExceptionInfo)
               (p/diverse-strong nil))))
