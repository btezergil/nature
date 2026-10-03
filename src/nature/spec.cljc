(ns nature.spec
  "Common specs/api checks for core nature functions"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [nature.panel-selectors :as panel-selectors]))

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
(s/def ::collaboration-mode #{:balanced :cartesian :panel :oracle})
(s/def ::panel-reference
  (s/keys :req-un [::guid ::genetic-sequence]))
(s/def ::panel
  (s/and (s/coll-of ::panel-reference :kind vector? :min-count 1)
         #(apply distinct? (map :guid %))))
(s/def ::panels (s/map-of ::species-id ::panel :min-count 2 :max-count 2))
(s/def ::next-panels (s/nilable ::panels))
(s/def ::panel-provenance map?)
(s/def ::next-panel-provenance (s/nilable map?))
(s/def ::average-score panel-selectors/finite-number?)
(s/def ::maximum-score panel-selectors/finite-number?)
(s/def ::encounter-count pos-int?)
(s/def ::panel-statistic
  (s/keys :req-un [::fitness-score ::average-score ::maximum-score ::encounter-count]))
(s/def ::panel-statistics (s/map-of ::species-id (s/map-of ::guid ::panel-statistic)))
(s/def ::metric-value panel-selectors/finite-number?)
(s/def ::source-generation nat-int?)
(s/def ::statistics ::panel-statistic)
(s/def ::opposite-panel-guids (s/coll-of ::guid :kind vector? :min-count 1))
(s/def ::champion
  ;; :individual is a scored snapshot, unlike bootstrap panel references.
  (s/keys :req-un [::individual ::metric-value ::source-generation
                   ::statistics ::opposite-panel-guids]))
(s/def ::fitness ::champion)
(s/def ::average ::champion)
(s/def ::maximum ::champion)
(s/def ::champions (s/keys :req-un [::fitness ::average ::maximum]))
(s/def ::panel-history (s/map-of ::species-id ::champions))
(s/def ::focal-species-id ::species-id)
(s/def ::focal-guid ::guid)
(s/def ::collaborator-species-id ::species-id)
(s/def ::collaborator-guid ::guid)
(s/def ::directional-collaboration
  (s/and ::collaboration
         (s/keys :req-un [::focal-species-id ::focal-guid
                          ::collaborator-species-id ::collaborator-guid])
         #(and (not= (:focal-species-id %) (:collaborator-species-id %))
               (= (:focal-guid %) (get (:participants %) (:focal-species-id %)))
               (= (:collaborator-guid %) (get (:participants %) (:collaborator-species-id %))))))
(s/def ::directional-collaboration-count nat-int?)
(s/def ::unique-collaboration-evaluation-count nat-int?)
(s/def ::panel-state
  (s/and
   (s/keys :req-un [::panels ::next-panels ::panel-provenance ::next-panel-provenance
                    ::panel-statistics ::panel-history ::directional-collaboration-count
                    ::unique-collaboration-evaluation-count])
   #(every? (partial s/valid? ::directional-collaboration) (:collaborations %))
   #(= (:directional-collaboration-count %) (count (:collaborations %)))
   #(<= (:unique-collaboration-evaluation-count %) (:directional-collaboration-count %))))

(s/def ::reference-id (s/and string? #(not (str/blank? %))))
(s/def ::reference-kind #{:oracle})
(s/def ::oracle-reference (s/keys :req-un [::reference-id]))
(s/def ::oracle-reference-metadata
  (s/map-of ::species-id ::oracle-reference :min-count 2 :max-count 2))
(s/def ::oracle-statistics ::panel-statistics)
(s/def ::oracle-evaluation-count nat-int?)
(s/def ::oracle-collaboration
  (s/and ::collaboration
         (s/keys :req-un [::focal-species-id ::focal-guid ::reference-id ::reference-kind])
         #(and (= {(:focal-species-id %) (:focal-guid %)} (:participants %))
               (= #{(:focal-species-id %)} (set (keys (:genomes %))))
               (panel-selectors/finite-number? (:score %))
               (not-any? (partial contains? %) [:collaborator-guid :collaborator-species-id]))))
(s/def ::oracle-state
  (s/and
   (s/keys :req-un [::oracle-reference-metadata ::oracle-statistics
                    ::oracle-evaluation-count ::directional-collaboration-count
                    ::unique-collaboration-evaluation-count])
   #(= (set (keys (:populations %))) (set (keys (:oracle-reference-metadata %)))
        (set (keys (:oracle-statistics %))))
   #(every? (partial s/valid? ::oracle-collaboration) (:collaborations %))
   #(= (:oracle-evaluation-count %) (:directional-collaboration-count %)
        (:unique-collaboration-evaluation-count %) (count (:collaborations %))
        (reduce + (map count (vals (:populations %)))))
   #(= (set (for [[id population] (:populations %) individual population]
               [id (:guid individual)]))
        (set (map (juxt :focal-species-id :focal-guid) (:collaborations %))))
   #(every? (fn [row]
              (= (:reference-id row)
                 (get-in % [:oracle-reference-metadata (:focal-species-id row) :reference-id])))
            (:collaborations %))))

(defn- valid-mode-state? [state]
  (and (or (not (contains? state :collaboration-mode))
           (s/valid? ::collaboration-mode (:collaboration-mode state)))
       (case (:collaboration-mode state)
         :panel (s/valid? ::panel-state state)
         :oracle (s/valid? ::oracle-state state)
         true)))

(s/def ::coevolution-state
  (s/and (s/keys :req-un [::generation ::populations ::collaborations])
         valid-mode-state?))
(s/def ::coevolution-result
  (s/and (s/keys :req-un [::generation ::populations ::collaborations
                         ::solutions ::final-collaborations])
         valid-mode-state?))
