(ns nature.core
  (:require [nature.spec :as s]
            [nature.population-presets :as pp]
            [nature.population-operators :as po])
  (:gen-class))

(defn uuid
  "More idiomatic wrapper around Java's v1 UUID functionality"
  []
  (str (java.util.UUID/randomUUID)))

(defn generate-sequence
  "Creates a genetic sequence of `sequence-length` elements,
  where each item is in the collection of `alleles`"
  [alleles sequence-length]
  (repeatedly sequence-length #(rand-nth alleles)))

(defn build-individual
  "Generate a new individual, and evaluate the fitness of the genetic sequence."
  ([genetic-sequence fitness-function]
   (assoc {} :genetic-sequence genetic-sequence
          :guid (uuid)
          :parents pp/initializer-name
          :age pp/default-age
          :fitness-score (fitness-function genetic-sequence)))

  ([genetic-sequence parent-coll age fitness-function]
   (assoc {} :genetic-sequence genetic-sequence
          :guid (uuid)
          :parents parent-coll
          :age age
          :fitness-score (fitness-function genetic-sequence))))

(defn build-population
  "Build `population-size` individuals by invoking `build-individual` on random, conforming genetic sequences."
  [population-size alleles sequence-length fitness-function]
  (repeatedly population-size #(build-individual (generate-sequence alleles sequence-length) fitness-function)))

(defn evolve
  "Create and evolve a population under the specified conditions until a termination criteria is reached
  `allele-set` is a collection of legal genome values
  `genome-length` is the enforced size of each genetic sequence
  `population-size` is the enforced number of individuals that will be created
  `generations` is the number of iterations the algorithm will cycle through
  `fitness-function` is a partial function accepting generated sequences to evaluate solution qualities
  `binary-operators` is a collection of partial functions accepting and returning 1 or more individuals
  `unary-operators` is a collection of partial functions accepting and returning exactly 1 individual
  `options` an optional map of pre-specified keywords to values that further tune the behavior of nature.
      Current examples follow:
      `:carry-over` an integer representing the top n individuals to be carried over between each generation. Default is 1
      `:solutions` an integer representing the top n individuals to return after evolution completes. Default is 1"
  ([allele-set genome-length population-size generations fitness-function binary-operators unary-operators]
   (evolve allele-set genome-length population-size generations fitness-function binary-operators unary-operators {:solutions 1, :carry-over 1}))
  ([allele-set genome-length population-size generations fitness-function binary-operators unary-operators options]
   (let [solutions (max 1 (:solutions options))
         carry-over (max 1 (:carry-over options))]
     (loop [population (build-population population-size allele-set genome-length fitness-function)
            current-generation 0]
       (if (>= current-generation generations)
         (take solutions (sort-by :fitness-score #(> %1 %2) population))
         (recur (po/advance-generation population binary-operators unary-operators {:carry-over carry-over}) (inc current-generation)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "¯\\_(ツ)_/¯"))
