(ns nature.core
  (:require [nature.initialization-operators :as io]
            [nature.population-operators :as po]
            [nature.coevolution :as coevolution]
            [nature.monitors :as monitors]
            #?(:clj [clojure.tools.logging :as log])))

(defn- all-zero-fitness?
  [population]
  (every? #(zero? (:fitness-score %)) population))

(defn- build-population-with-initial-retries
  [population-size generator-function fitness-function initial-population-retries]
  (loop [attempt 0
         population (io/build-population population-size generator-function fitness-function)]
    (if (and (< attempt initial-population-retries)
             (all-zero-fitness? population))
      (do
        #?(:clj
           (log/warn "Initial population has all-zero fitness; rebuilding before weighted selection."
                     {:attempt (inc attempt)
                      :initial-population-retries initial-population-retries})
           :cljs
           (.warn js/console
                  "Initial population has all-zero fitness; rebuilding before weighted selection."
                  (clj->js {:attempt (inc attempt)
                            :initial-population-retries initial-population-retries})))
        (recur (inc attempt)
               (io/build-population population-size generator-function fitness-function)))
      (do
        (when (and (pos? initial-population-retries)
                   (all-zero-fitness? population))
          (throw (ex-info "Initial population has all-zero fitness after configured retries; weighted selection cannot proceed."
                          {:population-size population-size
                           :initial-population-retries initial-population-retries
                           :attempts (inc attempt)})))
        population))))

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
  `:solutions` an integer representing the top n individuals to return after evolution completes. Default is 1
  `:monitors` a sequence of functions, assumed to be side-effectful, to be executed against `population` and `current-genration` for run-time stats. Default is nil"
  ([allele-set genome-length population-size generations fitness-function binary-operators unary-operators]
   (evolve allele-set genome-length population-size generations fitness-function binary-operators unary-operators {:solutions 1, :carry-over 1}))

  ([allele-set genome-length population-size generations fitness-function binary-operators unary-operators options] ;; TODO - Curry the genetic operators one more level, so the fitness-function can be pressed in
   {:pre [(and (every? coll? [allele-set binary-operators unary-operators])
               (every? int? [genome-length population-size generations])
               (fn? fitness-function))]}
   (let [solutions (max 1 (:solutions options))
         carry-over (max 1 (:carry-over options))
         monitors (:monitors options)]
     (loop [population (io/build-population population-size allele-set genome-length fitness-function)
            current-generation 0]
       (when monitors (monitors/apply-monitors monitors population current-generation))
       (if (>= current-generation generations)
         (take solutions (sort-by :fitness-score #(> %1 %2) population))
         (recur (po/advance-generation population population-size binary-operators unary-operators {:carry-over carry-over}) (inc current-generation)))))))

(defn evolve-with-sequence-generator
  "Same with evolve method, but takes a sequence generator function instead of an allele set and genome length.
  This method uses the sequence generator function to generate sequences for the initial population.
  `:initial-population-retries` optionally rebuilds the initial population when every fitness score is zero. Default is 0."
  ([generator-function population-size generations fitness-function binary-operators unary-operators]
   (evolve-with-sequence-generator generator-function population-size generations fitness-function binary-operators unary-operators {:solutions 1, :carry-over 1, :insert-new 0}))

  ([generator-function population-size generations fitness-function binary-operators unary-operators options]
   {:pre [(and (every? coll? [binary-operators unary-operators])
               (every? int? [population-size generations])
               (every? fn? [generator-function fitness-function]))]}
   (let [solutions (max 1 (get options :solutions 1))
         carry-over (max 1 (get options :carry-over 1))
         insert-new (max 0 (get options :insert-new 0))
         initial-population-retries (max 0 (get options :initial-population-retries 0))
         monitors (:monitors options)]
     (loop [population (build-population-with-initial-retries population-size generator-function fitness-function initial-population-retries)
            current-generation 0]
       (when monitors (monitors/apply-monitors monitors population current-generation))
       #?(:clj (log/info "generation #" current-generation)
          :cljs (.info js/console "generation #" current-generation))
       (if (>= current-generation generations)
         (take solutions (sort-by :fitness-score #(> %1 %2) population))
         (recur (po/advance-generation population population-size generator-function fitness-function binary-operators unary-operators {:carry-over carry-over :insert-new insert-new}) (inc current-generation)))))))

(defn evolve-cooperatively
  "Evolve two populations whose contextual fitness comes from collaboration.

  Each species configuration contains `:species-id`, `:population-size`,
  `:genome-generator`, `:binary-operators`, and `:unary-operators`; optional
  `:carry-over` and `:insert-new` values default to 1 and 0. Binary operators
  accept two genomes and return one or more child genomes. Unary operators
  accept and return one genome. `collaboration-fitness-fn` accepts one genome
  from each species (in argument order) and returns a numeric score.

  Options are `:collaboration-mode` (`:balanced`, the default, or `:cartesian`),
  `:opponents` (K for balanced scheduling, default 1), `:final-ratio` (default
  1.0), `:final-evaluation-fn`, and `:monitors`. Final evaluators accept the two
  genomes and may return any value. Each monitor accepts the complete state map."
  ([species-a species-b generations collaboration-fitness-fn]
   (evolve-cooperatively species-a species-b generations collaboration-fitness-fn {}))
  ([species-a species-b generations collaboration-fitness-fn options]
   (coevolution/evolve species-a species-b generations collaboration-fitness-fn options)))
