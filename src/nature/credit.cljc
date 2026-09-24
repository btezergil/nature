(ns nature.credit
  "Scalar credit for focal panel encounters. Higher credit is better."
  (:require [nature.panel-selectors :as selectors]))

(defn- require! [condition message data]
  (when-not condition (throw (ex-info message data))))

(defn- scores [{:keys [encounters]}]
  (let [values (mapv :score encounters)]
    (require! (and (seq values) (every? selectors/finite-number? values))
              "Credit requires non-empty finite encounter scores." {:scores values})
    values))

(defn- finite-result [value]
  (require! (selectors/finite-number? value)
            "Panel credit must return a finite number." {:credit value})
  value)

(defn mean
  "Arithmetic mean of focal encounter scores; the default credit."
  [context]
  (let [values (scores context)]
    (finite-result (/ (reduce + values) (count values)))))

(defn maximum
  "Maximum focal encounter score."
  [context]
  (apply max (scores context)))

(defn top-two-mean
  "Mean of the two highest scores, or the sole score for a singleton panel."
  [context]
  (let [values (take 2 (sort > (scores context)))]
    (finite-result (/ (reduce + values) (count values)))))

(defn weighted
  "Return a credit callback weighting scores in descending order.
   Weights must be finite, non-negative, and start positive (singleton bootstrap).
   Truncate and renormalize weights for shorter panels; longer panels are errors."
  [weights]
  (require! (and (sequential? weights) (seq weights)
                 (every? #(and (selectors/finite-number? %) (not (neg? %))) weights)
                 (pos? (first weights)))
            "Credit weights must be a non-empty finite non-negative sequence with a positive first weight."
            {:weights weights})
  (let [weights (vec weights)]
    (fn [context]
      (let [values (sort > (scores context))
            n (count values)]
        (require! (<= n (count weights))
                  "Credit weights must cover every actual panel encounter."
                  {:encounter-count n :weight-count (count weights)})
        (let [retained (subvec weights 0 n)
              total (reduce + retained)]
          (require! (and (selectors/finite-number? total) (pos? total))
                    "Retained credit weights must have a finite positive sum."
                    {:weights retained})
          (finite-result (reduce + (map #(* %1 (/ %2 total)) values retained))))))))

(defn resolve-options
  "Validate panel-only evolution options before initialization. Returns callback
   and serializable policy metadata; custom callback code is consumer-owned."
  [options]
  (let [policy (get options :panel-credit :mean)
        weighted? (= policy :weighted)]
    (require! (or weighted? (not (contains? options :panel-credit-weights)))
              ":panel-credit-weights is only supported with :panel-credit :weighted." {})
    (let [f (case policy
              :mean mean
              :maximum maximum
              :top-two-mean top-two-mean
              :weighted (weighted (:panel-credit-weights options))
              (do (require! (fn? policy) "Unknown :panel-credit policy." {:panel-credit policy})
                  policy))]
      {:credit-fn f
       :metadata (cond-> {:panel-credit-policy (if (fn? policy) :custom policy)}
                   weighted? (assoc :panel-credit-weights (vec (:panel-credit-weights options))))})))

(defn assign
  "Invoke a callback once per focal individual and require a finite scalar.
   Exceptions carry generation, species, and focal GUID for diagnosis."
  [credit-fn context]
  (let [error-context {:generation (:generation context)
                       :species-id (:species-id context)
                       :focal-guid (get-in context [:individual :guid])}]
    (try
      (finite-result (credit-fn context))
      (catch #?(:clj Exception :cljs :default) e
        (throw (ex-info "Panel credit assignment failed."
                        (merge (ex-data e) error-context) e))))))
