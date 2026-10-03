(ns nature.oracle
  "Fixed-reference evaluation for independently scored species."
  (:require [clojure.string :as str]
            [nature.credit :as credit]
            [nature.panel :as panel]
            [nature.panel-selectors :as selectors]))

(defn- require! [condition message data]
  (when-not condition (throw (ex-info message data))))

(defn validate-options
  "Validate species callbacks, fixed reference metadata, and final pair evaluation."
  [species-ids {:keys [oracle-fitness-fns oracle-reference-metadata final-evaluation-fn]
                :as options}]
  (let [ids (set species-ids)]
    (require! (and (map? oracle-fitness-fns)
                   (= ids (set (keys oracle-fitness-fns)))
                   (every? fn? (vals oracle-fitness-fns)))
              ":oracle-fitness-fns must provide one genome-to-score function per species."
              {:species-ids ids})
    (require! (and (map? oracle-reference-metadata)
                   (= ids (set (keys oracle-reference-metadata)))
                   (every? #(and (map? %) (string? (:reference-id %))
                                 (not (str/blank? (:reference-id %))))
                           (vals oracle-reference-metadata)))
              ":oracle-reference-metadata must provide a non-blank string :reference-id per species."
              {:species-ids ids :oracle-reference-metadata oracle-reference-metadata})
    (require! (fn? final-evaluation-fn)
              "Oracle mode requires an explicit :final-evaluation-fn for ordinary pairs."
              {})
    (require! (not-any? #(contains? options %) [:opponents :panel-selection-fns])
              "Oracle mode uses fixed references, not :opponents or :panel-selection-fns."
              {})
    options))

(defn- encounter
  [generation species-id individual fitness-fn reference]
  (let [context {:generation generation :species-id species-id
                 :focal-guid (:guid individual) :reference-id (:reference-id reference)}]
    (try
      (let [score (fitness-fn (:genetic-sequence individual))]
        (require! (selectors/finite-number? score)
                  "Oracle fitness must return a finite number." (assoc context :score score))
        {:participants {species-id (:guid individual)}
         :genomes {species-id (:genetic-sequence individual)}
         :focal-species-id species-id :focal-guid (:guid individual)
         :reference-kind :oracle :reference-id (:reference-id reference)
         :score score})
      (catch #?(:clj Exception :cljs :default) e
        (throw (ex-info "Oracle fitness evaluation failed."
                        (merge (ex-data e) context) e))))))

(defn evaluate
  "Score every live individual once; fixed references never receive focal credit."
  [populations fitness-fns references credit-fn generation]
  (panel/validate-populations populations)
  (let [jobs (for [[id population] populations individual population] [id individual])
        records (vec (#?(:clj pmap :cljs map)
                       (fn [[id individual]]
                         (encounter generation id individual (get fitness-fns id)
                                    (get references id))) jobs))
        by-focal (into {} (map (juxt (juxt :focal-species-id :focal-guid) identity) records))
        statistics
        (into {}
              (for [[id population] populations]
                [id (into {}
                          (for [individual population
                                :let [record (get by-focal [id (:guid individual)])
                                      assigned (credit/assign credit-fn
                                                 {:generation generation :collaboration-mode :oracle
                                                  :species-id id :individual individual
                                                  :oracle-reference (get references id)
                                                  :encounters [record]})]]
                            [(:guid individual) {:fitness-score assigned
                                                 :average-score (:score record)
                                                 :maximum-score (:score record)
                                                 :encounter-count 1}]))]))]
    {:populations (into {} (for [[id population] populations]
                            [id (mapv #(assoc % :fitness-score
                                               (get-in statistics [id (:guid %) :fitness-score]))
                                      population)]))
     :collaborations records
     :oracle-reference-metadata references
     :oracle-statistics statistics
     :oracle-evaluation-count (count records)
     :directional-collaboration-count (count records)
     :unique-collaboration-evaluation-count (count records)}))
