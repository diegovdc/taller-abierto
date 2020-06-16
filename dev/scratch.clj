(ns scratch
  (:require [clojure.spec.alpha :as s]))

(s/def ::bounds (s/and vector? (s/coll-of number?) #(>= (count %) 2)))
(s/def ::init number?)
(s/def ::f fn?)
(s/def ::bound-with fn?)
(s/def ::param-generator (s/keys :req-un [::init ::f]
                                 :opt-un [::bounds ::bound-with]))

(defn attract [f to val]
  (cond
    (> val to) (- val (f to val))
    (< val to) (+ val (f to val))
    :is-equal val))

(defn bound
  "Return a numeric value within min and max,
  by folding the value once it reaches either extreme."
  [min max val]
  (cond
    (< val min) min
    (and (<= min val) (>= max val)) val
    (> val max) (let [amount (mod val (- max min))]
                  (if (even? (int (quot val max)))
                    #_:up (+ min amount)
                    #_:down (- max amount)))))

(defn next-state-unindexed [prev-state param-fns _]
  (assoc prev-state :unbounded-params
         (reduce (fn [params [param {:keys [init f]}]]
                   (let [prev-val (or (params param) init)]
                     (assoc params param (f prev-val))))
                 (get prev-state :unbounded-params {})
                 param-fns)))

(defn next-state-indexed [prev-state param-fns index]
  (assoc prev-state :unbounded-params
         (reduce (fn [params [param {:keys [init f]}]]
                   (let [prev-val (or (params param) init)]
                     (assoc params param (f index prev-val))))
                 (get prev-state :unbounded-params {})
                 param-fns)))

(defn next-state-indexed [prev-state param-fns index]
  (assoc prev-state :unbounded-params
         (reduce (fn [params [param {:keys [init f]}]]
                   (let [prev-val (or (params param) init)]
                     (assoc params param (f index prev-val))))
                 (get prev-state :unbounded-params {})
                 param-fns)))

(defn bound-params [prev-state bound-fn param-fns]
  (->> param-fns
       (reduce
        (fn [prev-state [param {:keys [bounds bound-with]}]]
          (let [param-val (-> prev-state :unbounded-params param)
                bounds* #(-> (into [] bounds) (subvec 0 2))
                b-param-val
                ,(cond
                   (s/valid? ::bounds bounds) (->> param-val
                                                   (conj (bounds*))
                                                   (apply bound-fn))
                   (fn? bound-with) (try (bound-with param-val)
                                         (catch Exception _ param-val))
                   :default param-val)]
            (assoc-in prev-state [:params param] b-param-val)))
        prev-state)))

(defn inc-param-events [prev-state params index]
  (reduce #(-> %1 (update-in [:events-count %2 index] (fn [n] (inc (or n 0)))))
          (->  prev-state
               (assoc :last-index index)
               (update-in [:events-count :total-events] (fn [n] (inc (or n 0)))))
          params))

(defn flocking
  [param-fns prev-state index]
  (let [ps  @prev-state
        next-param-state-fn (let [f (get param-fns :flocking/fn)]
                              (case f
                                fn? f
                                :unindexed next-state-unindexed
                                next-state-indexed))
        index* (cond
                 (= :total (get param-fns :flocking/index-mode))
                 (get-in ps [:events-count :total-events] 0)
                 :else index)
        bound-fn (get param-fns :flocking/bound-fn bound)
        param-fns* (filter
                    (fn [[_ v]] (s/valid? ::param-generator v))
                    param-fns)
        params (keys param-fns*)
        new-state (-> ps
                      (inc-param-events params index)
                      (next-param-state-fn param-fns* index*)
                      (bound-params bound-fn param-fns*))]
    (reset! prev-state new-state)))


(comment
  (require '[lorentz :as l])
  (def lor (l/init-system))
  (defn l-coord
    ([coord min max] (l-coord coord min max 0))
    ([coord min max offset]
     (fn ([i _] (-> i (+ offset) lor (l/bound coord min max))))))
  (def state (atom nil))
  (def i (atom 0))
  (-> @state :events-count :total-events)
  (do
    (swap! i inc)
    (flocking {:flocking/index-mode :total
               :rate {:init 1 :f (l-coord :x 0.3 8)}
               :amp {:init 0.5 :f #(+ (* 0.1 %1) %2) :bounds [0.25 1.5]}}
              state
              @i)))
