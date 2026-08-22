(ns nature.spec
  "Common specs/api checks for core nature functions"
  (:require [clojure.spec.alpha :as s]))

(defn not-empty?
  "A predicate version of not-empty, because it's a sensible feature"
  [coll]
  (boolean (seq coll)))

(s/def ::genetic-sequence
  (s/and coll?
         not-empty?))

(s/def ::guid string?)

(s/def ::parents
  (s/and coll?
         not-empty?))

(s/def ::age integer?)

(s/def ::fitness-score number?)

(s/def ::individual
  (s/keys :req-un [::genetic-sequence
                   ::guid
                   ::parents
                   ::age
                   ::fitness-score]))

(s/def ::population
  (s/and #(s/coll-of (s/valid? ::individual %))
         not-empty?))

(s/def ::species-id keyword?)
(s/def ::population-size pos-int?)
(s/def ::genome-generator fn?)
(s/def ::binary-operators (s/coll-of fn?))
(s/def ::unary-operators (s/coll-of fn?))
(s/def ::carry-over nat-int?)
(s/def ::insert-new nat-int?)

(s/def ::species-configuration
  (s/keys :req-un [::species-id
                   ::population-size
                   ::genome-generator
                   ::binary-operators
                   ::unary-operators]
          :opt-un [::carry-over ::insert-new]))

(s/def ::participants map?)
(s/def ::genomes map?)
(s/def ::score number?)
(s/def ::collaboration
  (s/keys :req-un [::participants ::genomes ::score]))
(s/def ::collaborations (s/coll-of ::collaboration :kind vector?))
(s/def ::generation nat-int?)
(s/def ::populations map?)
(s/def ::solutions map?)
(s/def ::result (constantly true))
(s/def ::final-collaboration
  (s/keys :req-un [::participants ::result]))
(s/def ::final-collaborations
  (s/coll-of ::final-collaboration :kind vector?))
(s/def ::coevolution-state
  (s/keys :req-un [::generation ::populations ::collaborations]))
(s/def ::coevolution-result
  (s/keys :req-un [::generation ::populations ::collaborations
                   ::solutions ::final-collaborations]))
