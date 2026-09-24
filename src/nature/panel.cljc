(ns nature.panel
  "Internal panel scheduling, validation, statistics, and bounded history."
  (:require [nature.panel-selectors :as selectors]
            [nature.credit :as credit]))

(defn- require! [condition message data]
  (when-not condition (throw (ex-info message data))))

(defn validate-selectors [selection-fns]
  (require! (and (sequential? selection-fns) (seq selection-fns)
                 (every? fn? selection-fns))
            ":panel-selection-fns must be a non-empty sequence of functions."
            {:panel-selection-fns selection-fns})
  selection-fns)

(defn valid-reference? [individual]
  (and (map? individual) (string? (:guid individual))
       (coll? (:genetic-sequence individual))
       (seq (:genetic-sequence individual))))

(defn- validate-references [species-id individuals]
  (doseq [individual individuals]
    (require! (valid-reference? individual)
              "Panel candidates require a string GUID and a non-empty genome."
              {:species-id species-id :individual individual}))
  (doseq [[guid snapshots] (group-by :guid individuals)]
    (require! (apply = (map :genetic-sequence snapshots))
              "A species GUID has conflicting genomes."
              {:species-id species-id :guid guid})))

(defn validate-populations [populations]
  (doseq [[id population] populations]
    (require! (seq population) "Panel populations must be non-empty." {:species-id id})
    (validate-references id population)
    (require! (= (count population) (count (set (map :guid population))))
              "Live population GUIDs must be distinct within a species."
              {:species-id id})))

(defn bootstrap [populations]
  (validate-populations populations)
  (let [panels (into {} (map (fn [[id population]]
                              [id [(rand-nth population)]]) populations))]
    {:panels panels
     :panel-provenance
     (into {} (map (fn [[id panel]]
                    [id {(:guid (first panel)) [{:selector-name :bootstrap}]}])
                  panels))}))

(defn assemble
  "Build one species panel, preserving first snapshots and all selector origins."
  [selection-fns {:keys [species-id population history previous-panel] :as context}]
  (validate-selectors selection-fns)
  (let [sources (vec (concat population (map :individual (vals history)) previous-panel))
        allowed (set sources)]
    (validate-references species-id sources)
    (let [result
          (reduce
           (fn [{:keys [panel provenance]} [index selector]]
             (let [error-context (select-keys context [:generation :source-generation :species-id])
                   error-context (assoc error-context :selector-index index)
                   output (try
                            (let [returned (selector (assoc context :selected-panel panel))]
                              (require! (sequential? returned)
                                        "A panel selector must return a sequence." error-context)
                              (vec returned))
                            (catch #?(:clj Exception :cljs :default) e
                              (throw (ex-info "Panel selector failed."
                                              (merge (ex-data e) error-context) e))))
                   origin {:selector-index index
                           :selector-name (or (:panel-selector-name (meta selector)) :custom)}]
               (reduce (fn [{:keys [panel provenance]} individual]
                         (require! (contains? allowed individual)
                                   "Panel selector returned an unknown or modified individual."
                                   (assoc error-context :individual individual))
                         (let [guid (:guid individual)]
                           {:panel (if (contains? provenance guid) panel (conj panel individual))
                            :provenance (update provenance guid (fnil conj []) origin)}))
                       {:panel panel :provenance provenance} output)))
           {:panel [] :provenance {}}
           (map-indexed vector selection-fns))]
      (require! (seq (:panel result)) "The assembled panel must be non-empty."
                (select-keys context [:generation :species-id]))
      result)))

(defn next-panels [selection-fns species state]
  (let [built
        (into {}
              (for [[id configuration] species]
                [id (assemble selection-fns
                              {:generation (inc (:generation state))
                               :source-generation (:generation state)
                               :species-id id :species configuration
                               :population (get-in state [:populations id])
                               :statistics (get-in state [:panel-statistics id])
                               :history (get-in state [:panel-history id])
                               :previous-panel (get-in state [:panels id])})]))]
    {:panels (into {} (map (fn [[id result]] [id (:panel result)]) built))
     :panel-provenance (into {} (map (fn [[id result]] [id (:provenance result)]) built))}))

(defn evaluate
  ([id-a id-b populations panels fitness-fn]
   (evaluate id-a id-b populations panels fitness-fn credit/mean nil))
  ([id-a id-b populations panels fitness-fn credit-fn generation]
  (validate-populations populations)
  (doseq [id [id-a id-b]]
    (let [panel (get panels id)]
      (require! (and (sequential? panel) (seq panel))
                "Both species require non-empty panels." {:species-id id})
      (require! (= (count panel) (count (set (map :guid panel))))
                "Panel GUIDs must be distinct." {:species-id id})
      (validate-references id (concat (get populations id) panel))))
  (let [encounters (vec (concat
                         (for [a (get populations id-a) b (get panels id-b)] [a b id-a])
                         (for [b (get populations id-b) a (get panels id-a)] [a b id-b])))
        pair-key (fn [[a b]] [(:guid a) (:guid b)])
        pairs (vals (reduce (fn [m encounter] (assoc m (pair-key encounter) encounter))
                            {} encounters))
        scores (into {}
                     (map (fn [[key score :as entry]]
                            (require! (selectors/finite-number? score)
                                      "Panel fitness must return a finite number."
                                      {:participants key :score score})
                            entry))
                     (#?(:clj pmap :cljs map)
                      (fn [[a b :as pair]]
                        [(pair-key pair)
                         (fitness-fn (:genetic-sequence a) (:genetic-sequence b))])
                      pairs))
        records (mapv (fn [[a b focal :as encounter]]
                        (let [a-focal? (= focal id-a)]
                          {:participants {id-a (:guid a) id-b (:guid b)}
                           :genomes {id-a (:genetic-sequence a) id-b (:genetic-sequence b)}
                           :focal-species-id focal
                           :focal-guid (:guid (if a-focal? a b))
                           :collaborator-species-id (if a-focal? id-b id-a)
                           :collaborator-guid (:guid (if a-focal? b a))
                           :score (get scores (pair-key encounter))}))
                      encounters)
        grouped (group-by (juxt :focal-species-id :focal-guid) records)
        statistics
        (into {}
              (for [[id population] populations]
                [id (into {}
                          (for [individual population
                                :let [encounters (get grouped [id (:guid individual)])
                                      values (map :score encounters)
                                      mean (/ (reduce + values) (count values))]]
                            (do
                              (require! (selectors/finite-number? mean)
                                        "Panel mean fitness must be finite."
                                        {:species-id id :guid (:guid individual)})
                              [(:guid individual) {:fitness-score
                                                   (credit/assign credit-fn
                                                     {:generation generation :species-id id
                                                      :individual individual
                                                      :collaborator-species-id (if (= id id-a) id-b id-a)
                                                      :panel (get panels (if (= id id-a) id-b id-a))
                                                      :encounters encounters})
                                                   :average-score mean
                                                   :maximum-score (apply max values)
                                                   :encounter-count (count values)}])))]))]
    {:populations (into {} (for [[id population] populations]
                             [id (mapv #(assoc % :fitness-score
                                               (get-in statistics [id (:guid %) :fitness-score]))
                                       population)]))
     :collaborations records
     :panel-statistics statistics
     :directional-collaboration-count (count records)
     :unique-collaboration-evaluation-count (count scores)})))

(defn update-history [history state]
  (reduce
   (fn [history [id population]]
     (let [other-id (first (remove #{id} (keys (:populations state))))]
       (reduce
        (fn [history [metric stat-key]]
          (reduce
           (fn [history individual]
             (let [stats (get-in state [:panel-statistics id (:guid individual)])
                   value (get stats stat-key)
                   previous (get-in history [id metric])]
               (if (or (nil? previous) (> value (:metric-value previous)))
                 (assoc-in history [id metric]
                           {:individual individual :metric-value value
                            :source-generation (:generation state) :statistics stats
                            :opposite-panel-guids (mapv :guid (get-in state [:panels other-id]))})
                 history)))
           history population))
        history [[:fitness :fitness-score] [:average :average-score] [:maximum :maximum-score]])))
   (or history {}) (:populations state)))
