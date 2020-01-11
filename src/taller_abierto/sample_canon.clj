(ns taller-abierto.sample-canon
  (:require
   [overtone.core :refer [metronome node-status with-inactive-node-modification-error]]
   [taller-abierto.instruments :as i]
   [taller-abierto.standard :refer [get-instruments get-synth xo-play?]]
   [time-time.sequencing :refer [sequencer]]
   [time-time.standard :refer [nthw rand-pos]]))

(defn voice-filter
  "The `state` may have a `:voicef` key that may pass a `set` of voice
  indexes, a function that takes an index and returns a boolean or a boolean.
  See `smpl-playa` for use."
  [voices tempo-index]
  (cond
    (instance? clojure.lang.IFn voices) (voices tempo-index)
    (boolean? voices) voices
    :default true))

(defn maybe-filter-freed-synths
  "Using the state's `[:cleanup :every]`
  and `[:cleanup :newly-played-synths]` keys
  the function removes the non-active synths from the :playing-synths list.

  The idea is not to apply this filtering every time, to avoid
  the performance overhead.

  `:playing-synths` may be used to apply `ctl` values to the synths"
  [derefed-state]
  (if (> (get-in derefed-state [:cleanup :newly-played-synths])
         (get-in derefed-state [:cleanup :every] 5))
    (-> derefed-state (assoc :playing-synths
                             (filter (fn [s] (= :destroyed (node-status s)))
                                     (derefed-state :playing-synths)))
        (assoc-in [:cleanup :newly-played-synths] 0))
    derefed-state))

(defn update-playing-synths
  [derefed-state synth]
  (-> derefed-state
      (update-in [:cleanup :newly-played-synths] #(inc (or % 0)))
      ;; maybe-filter-freed-synths
      (update :playing-synths conj synth)))
(def at (atom nil))
(-> at deref :playing-synths)
(swap! at update-playing-synths 1)

(defn ctl-list [state ctl-fn]
  (let [synths (@state :playing-synths)]
    (with-inactive-node-modification-error :warning
      (doseq [s synths] (ctl-fn s)))))

(defn smpl-playa [vals index nome state sample-sequence pan]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx) (nthw (get-instruments state) index i/silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))
        synth* (get-synth state)]
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))
    (when (and
           (voice-filter (@state :voicef) (vals :tempo-index))
           (xo-play? state index))
      (swap! state #'update-playing-synths
             (with-meta (synth* :vals vals
                                :metronome nome
                                :index index
                                :sample smpl
                                :start-pos start-pos
                                :pan pan)
               {:data (assoc vals :index index)})))))

(defn sample-canon
  [state canon & {:keys [pan nome] :or {pan 0 nome (metronome 60)}}]
  (let [sample-sequence (atom {})]
    (->> canon
         (mapv (fn [voice]
                 (sequencer
                  nome
                  voice
                  (fn [vals index]
                    (#'smpl-playa
                     vals
                     index
                     nome
                     state
                     sample-sequence
                     pan))))))))