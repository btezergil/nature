(ns nature.coevolution
  "Implementation helpers for two-species cooperative coevolution."
  (:require [nature.initialization-operators :as io]
            [nature.population-presets :as pp]))

(defn- fail
  [message data]
  (throw (ex-info message data)))

(defn- require-condition
  [condition message data]
  (when-not condition
    (fail message data)))

(defn- validate-species
  [species]
  (let [{:keys [species-id population-size genome-generator binary-operators
                unary-operators carry-over insert-new]} species]
    (require-condition (keyword? species-id)
                       "A species configuration requires a keyword :species-id."
                       {:species species})
    (require-condition (and (int? population-size) (pos? population-size))
                       "A species :population-size must be a positive integer."
                       {:species-id species-id :population-size population-size})
    (require-condition (fn? genome-generator)
                       "A species :genome-generator must be a function."
                       {:species-id species-id})
    (require-condition (and (coll? binary-operators) (every? fn? binary-operators))
                       "A species :binary-operators must be a collection of functions."
                       {:species-id species-id})
    (require-condition (and (coll? unary-operators) (every? fn? unary-operators))
                       "A species :unary-operators must be a collection of functions."
                       {:species-id species-id})
    (require-condition (and (int? carry-over) (not (neg? carry-over)))
                       "A species :carry-over must be a non-negative integer."
                       {:species-id species-id :carry-over carry-over})
    (require-condition (and (int? insert-new) (not (neg? insert-new)))
                       "A species :insert-new must be a non-negative integer."
                       {:species-id species-id :insert-new insert-new})
    (require-condition (<= (+ carry-over insert-new) population-size)
                       "The sum of :carry-over and :insert-new cannot exceed :population-size."
                       {:species-id species-id
                        :population-size population-size
                        :carry-over carry-over
                        :insert-new insert-new})
    species))

(defn- normalize-species
  [species]
  (validate-species
   (merge {:binary-operators []
           :unary-operators []
           :carry-over 1
           :insert-new 0}
          species)))

(defn- unscored-individual
  [genome parents age]
  {:genetic-sequence genome
   :guid (io/uuid)
   :parents parents
   :age age
   :fitness-score nil})

(defn- initialize-population
  [{:keys [population-size genome-generator]}]
  (vec (repeatedly population-size
                   #(unscored-individual (genome-generator)
                                         pp/initializer-name
                                         pp/default-age))))

(defn- collaboration-pairs
  [population-a population-b mode opponents]
  (case mode
    :cartesian
    (vec (for [a population-a
               b population-b]
           [a b]))

    :balanced
    (let [size-a (count population-a)
          size-b (count population-b)]
      (require-condition (= size-a size-b)
                         "Balanced collaboration requires equally sized populations so every member can have exactly K opponents."
                         {:population-size-a size-a :population-size-b size-b})
      (require-condition (and (int? opponents) (pos? opponents))
                         "Balanced collaboration :opponents must be a positive integer."
                         {:opponents opponents})
      (require-condition (<= opponents size-a)
                         "Balanced collaboration :opponents cannot exceed either population size."
                         {:opponents opponents :population-size size-a})
      (let [shuffled-a (vec (shuffle population-a))
            shuffled-b (vec (shuffle population-b))]
        (vec (for [a-index (range size-a)
                   offset (range opponents)]
               [(nth shuffled-a a-index)
                (nth shuffled-b (mod (+ a-index offset) size-b))]))))

    (fail "Unknown collaboration mode."
          {:collaboration-mode mode :supported-modes #{:balanced :cartesian}})))

(defn- average
  [numbers]
  (/ (reduce + numbers) (count numbers)))

(defn- evaluate-populations
  [species-a species-b population-a population-b mode opponents fitness-fn]
  (let [id-a (:species-id species-a)
        id-b (:species-id species-b)
        pairs (collaboration-pairs population-a population-b mode opponents)
        collaborations
        (into []
              (#?(:clj pmap :cljs map)
               (fn [[individual-a individual-b]]
                 (let [score (fitness-fn (:genetic-sequence individual-a)
                                         (:genetic-sequence individual-b))]
                   (require-condition (number? score)
                                      "The collaboration fitness function must return a number."
                                      {:species-a id-a :species-b id-b :score score})
                   {:participants {id-a (:guid individual-a)
                                   id-b (:guid individual-b)}
                    :genomes {id-a (:genetic-sequence individual-a)
                              id-b (:genetic-sequence individual-b)}
                    :score score}))
               pairs))
        scores (reduce (fn [index {:keys [participants score]}]
                         (-> index
                             (update (get participants id-a) (fnil conj []) score)
                             (update (get participants id-b) (fnil conj []) score)))
                       {}
                       collaborations)
        assign-score (fn [individual]
                       (let [individual-scores (get scores (:guid individual))]
                         (require-condition (seq individual-scores)
                                            "Every individual must participate in at least one collaboration."
                                            {:guid (:guid individual)})
                         (assoc individual :fitness-score (average individual-scores))))]
    {:populations {id-a (mapv assign-score population-a)
                   id-b (mapv assign-score population-b)}
     :collaborations collaborations}))

(defn- selection-weights
  [population]
  (let [minimum (apply min (map :fitness-score population))
        shift (if (pos? minimum) 0 (- 1 minimum))]
    (mapv #(+ shift (:fitness-score %)) population)))

(defn- weighted-parent
  [population]
  (let [weights (selection-weights population)
        total (reduce + weights)
        target (* (rand) total)]
    (loop [remaining population
           remaining-weights weights
           cumulative 0]
      (let [candidate (first remaining)
            next-cumulative (+ cumulative (first remaining-weights))]
        (if (or (nil? (next remaining)) (>= next-cumulative target))
          candidate
          (recur (next remaining) (next remaining-weights) next-cumulative))))))

(defn- child-genomes
  [binary-operator parent-a parent-b species-id]
  (let [children (binary-operator (:genetic-sequence parent-a)
                                  (:genetic-sequence parent-b))]
    (require-condition (and (coll? children) (seq children) (every? coll? children))
                       "A cooperative binary operator must return one or more child genomes."
                       {:species-id species-id :returned children})
    children))

(defn- mutate-genome
  [genome unary-operators species-id]
  (if (seq unary-operators)
    (let [mutated ((rand-nth (vec unary-operators)) genome)]
      (require-condition (coll? mutated)
                         "A cooperative unary operator must return one genome."
                         {:species-id species-id :returned mutated})
      mutated)
    genome))

(defn- offspring
  [population number-needed {:keys [species-id binary-operators unary-operators]}]
  (require-condition (seq binary-operators)
                     "At least one binary operator is required when offspring must be created."
                     {:species-id species-id :offspring-needed number-needed})
  (loop [children []]
    (if (>= (count children) number-needed)
      (subvec (vec children) 0 number-needed)
      (let [parent-a (weighted-parent population)
            parent-b (weighted-parent population)
            operator (rand-nth (vec binary-operators))
            genomes (child-genomes operator parent-a parent-b species-id)
            new-children (mapv #(unscored-individual
                                 (mutate-genome % unary-operators species-id)
                                 [(:guid parent-a) (:guid parent-b)]
                                 pp/default-age)
                               genomes)]
        (recur (into children new-children))))))

(defn- advance-population
  [population {:keys [population-size genome-generator carry-over insert-new]
               :as species}]
  (let [elite (mapv #(-> % (update :age inc) (assoc :fitness-score nil))
                    (take carry-over (sort-by :fitness-score > population)))
        inserted (vec (repeatedly insert-new
                                  #(unscored-individual (genome-generator)
                                                        pp/initializer-name
                                                        pp/default-age)))
        needed (- population-size carry-over insert-new)]
    (vec (concat elite (when (pos? needed) (offspring population needed species)) inserted))))

(defn- monitor!
  [monitors state]
  (doseq [monitor monitors]
    (monitor state)))

(defn- top-count
  [population-size final-ratio]
  #?(:clj (long (Math/ceil (* population-size final-ratio)))
     :cljs (long (js/Math.ceil (* population-size final-ratio)))))

(defn- final-collaborations
  [species-a species-b populations final-ratio final-evaluation-fn collaboration-fitness-fn]
  (let [id-a (:species-id species-a)
        id-b (:species-id species-b)
        select (fn [id population-size]
                 (vec (take (top-count population-size final-ratio)
                            (sort-by :fitness-score > (get populations id)))))
        solutions-a (select id-a (:population-size species-a))
        solutions-b (select id-b (:population-size species-b))
        evaluator (or final-evaluation-fn collaboration-fitness-fn)
        results (vec
                 (for [individual-a solutions-a
                       individual-b solutions-b]
                   {:participants {id-a individual-a
                                   id-b individual-b}
                    :result (evaluator (:genetic-sequence individual-a)
                                       (:genetic-sequence individual-b))}))]
    {:solutions {id-a solutions-a id-b solutions-b}
     :final-collaborations results}))

(defn evolve
  [species-a species-b generations collaboration-fitness-fn options]
  (require-condition (and (int? generations) (not (neg? generations)))
                     "The generation count must be a non-negative integer."
                     {:generations generations})
  (require-condition (fn? collaboration-fitness-fn)
                     "The collaboration fitness function must be a function."
                     {})
  (let [species-a (normalize-species species-a)
        species-b (normalize-species species-b)
        id-a (:species-id species-a)
        id-b (:species-id species-b)
        mode (get options :collaboration-mode :balanced)
        opponents (get options :opponents 1)
        final-ratio (get options :final-ratio 1.0)
        final-evaluation-fn (:final-evaluation-fn options)
        monitors (or (:monitors options) [])]
    (require-condition (not= id-a id-b)
                       "Cooperating species must have distinct :species-id values."
                       {:species-id id-a})
    (require-condition (and (number? final-ratio) (pos? final-ratio) (<= final-ratio 1))
                       ":final-ratio must be greater than zero and no greater than one."
                       {:final-ratio final-ratio})
    (require-condition (or (nil? final-evaluation-fn) (fn? final-evaluation-fn))
                       ":final-evaluation-fn must be a function when supplied."
                       {})
    (require-condition (and (coll? monitors) (every? fn? monitors))
                       ":monitors must be a collection of functions."
                       {})
    (loop [generation 0
           population-a (initialize-population species-a)
           population-b (initialize-population species-b)]
      (let [{:keys [populations collaborations]}
            (evaluate-populations species-a species-b population-a population-b
                                  mode opponents collaboration-fitness-fn)
            state {:generation generation
                   :populations populations
                   :collaborations collaborations}]
        (monitor! monitors state)
        (if (>= generation generations)
          (merge state
                 (final-collaborations species-a species-b populations final-ratio
                                       final-evaluation-fn collaboration-fitness-fn))
          (recur (inc generation)
                 (advance-population (get populations id-a) species-a)
                 (advance-population (get populations id-b) species-b)))))))
