(ns nature.panel-selectors
  "Composable selectors for completed cooperative populations.
   Selectors take a context map and return individual snapshots.")

(defn finite-number?
  "True for finite numeric values (including ratios on the JVM)."
  [x]
  (and (number? x)
       #?(:clj (Double/isFinite (double x))
          :cljs (js/Number.isFinite x))))

(defn- named-selector [selector-name f]
  (with-meta f {:panel-selector-name selector-name}))

(defn- validate-count [n]
  (when-not (and (int? n) (pos? n))
    (throw (ex-info "Selector count must be a positive integer." {:count n}))))

(defn random-members
  "Select up to n distinct candidates uniformly without replacement."
  [n]
  (validate-count n)
  (named-selector :random
                  (fn [{:keys [population]}]
                    (vec (take n (shuffle population))))))

(def all-members
  "Select every member of the completed population."
  (named-selector :all-members (fn [{:keys [population]}] (vec population))))

(defn ranked-members
  "Select up to n members by descending score-fn (individual, context).
   Ties preserve population order."
  [n score-fn]
  (validate-count n)
  (when-not (fn? score-fn)
    (throw (ex-info "Ranking requires a score function." {})))
  (named-selector
   :ranked
   (fn [{:keys [population] :as context}]
     (->> population
          (mapv (fn [individual]
                  (let [score (score-fn individual context)]
                    (when-not (finite-number? score)
                      (throw (ex-info "Ranking score must be finite."
                                      {:guid (:guid individual) :score score})))
                    [individual score])))
          (sort-by second >)
          (take n)
          (mapv first)))))

(def best-fitness
  "Select the highest assigned-fitness member."
  (named-selector :best-fitness
                  (ranked-members 1 (fn [individual _] (:fitness-score individual)))))

(def specialist
  "Select the member with the highest single focal encounter score."
  (named-selector
   :specialist
   (ranked-members 1 (fn [individual context]
                      (get-in context [:statistics (:guid individual) :maximum-score])))))

(def generalist
  "Select the member with the highest mean focal encounter score."
  (named-selector
   :generalist
   (ranked-members 1 (fn [individual context]
                      (get-in context [:statistics (:guid individual) :average-score])))))

(defn- historical-selector [metric selector-name]
  (named-selector selector-name
                  (fn [{:keys [history]}]
                    (if-let [individual (get-in history [metric :individual])]
                      [individual]
                      []))))

(def historical-best-fitness
  "Select the retained best assigned-fitness observation."
  (historical-selector :fitness :historical-best-fitness))

(def historical-best-average
  "Select the retained best mean-score observation."
  (historical-selector :average :historical-best-average))

(def historical-best-maximum
  "Select the retained best single-encounter observation."
  (historical-selector :maximum :historical-best-maximum))

(defn diverse-strong
  "Select a maximin-distance member of the top fitness quartile.
   distance-fn receives two genomes and context and returns a finite,
   non-negative number. Zero means equivalent. Return [] if none is distinct."
  [distance-fn]
  (when-not (fn? distance-fn)
    (throw (ex-info "Diversity requires a structural distance function." {})))
  (named-selector
   :diverse-strong
   (fn [{:keys [population selected-panel] :as context}]
     (let [ranked (vec (sort-by :fitness-score > population))
           quartile (take (max 1 (quot (+ (count ranked) 3) 4)) ranked)
           references (if (seq selected-panel) selected-panel (take 1 ranked))
           selected (set (map :guid selected-panel))
           candidates
           (for [individual quartile
                 :when (not (selected (:guid individual)))]
             [individual
              (apply min
                     (map (fn [reference]
                            (let [distance (distance-fn (:genetic-sequence individual)
                                                        (:genetic-sequence reference)
                                                        context)]
                              (when-not (and (finite-number? distance)
                                             (not (neg? distance)))
                                (throw (ex-info "Structural distance must be finite and non-negative."
                                                {:distance distance
                                                 :guid (:guid individual)})))
                              distance))
                          references))])
           winner (first (sort-by second > (filter #(pos? (second %)) candidates)))]
       ;; Candidates already follow descending fitness and stable population order.
       (if winner [(first winner)] [])))))
