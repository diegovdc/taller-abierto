(ns taller-abierto.sample-canon
  (:require
   [clojure.spec.alpha :as s]
   [taller-abierto.graphs.specs :as gspecs]
   [taller-abierto.internal :refer [validate]]
   [time-time.converge :refer [canon-dur]]
   [visuals.durations :as v]
   [overtone.core :refer [metronome metro-bpm node-status with-inactive-node-modification-error] :as o]
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
         (get-in derefed-state [:cleanup :every] 100))
    (-> derefed-state (assoc :playing-synths
                             (filter (fn [s] (= :destroyed (node-status s)))
                                     (derefed-state :playing-synths)))
        (assoc-in [:cleanup :newly-played-synths] 0))
    derefed-state))

(defn update-playing-synths
  [derefed-state synth]
  (-> derefed-state
      (update-in [:cleanup :newly-played-synths] #(inc (or % 0)))
      maybe-filter-freed-synths
      (update :playing-synths conj synth)))

(defn ctl-list [state ctl-fn]
  (let [synths (@state :playing-synths)]
    (with-inactive-node-modification-error :silent
      (doseq [s synths] (ctl-fn s)))))

(defn- validate-synth! [synth*]
  (when-not (s/valid? ::gspecs/synth synth*)
    (throw (ex-info
            "Invalid synth:"
            {:synth synth*
             :explanation (s/explain-str ::gspecs/synth synth*)}))))


(defn smpl-playa [data index nome state sample-sequence pan]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx)
                 (nthw (get-instruments state) index i/silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))
        synth* (get-synth state)]
    (validate-synth! synth*)
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))
    (when (and
           (voice-filter (@state :voicef) (data :tempo-index))
           (xo-play? state index))
      (let [synth-node (try (synth* :data data
                                    :metronome nome
                                    :index index
                                    :sample smpl
                                    :start-pos start-pos
                                    :pan pan
                                    :state state)
                            (catch Exception e
                              (println "sample-play synth* Exception" e)))]
        (when synth-node
         (swap! state #'update-playing-synths
                (with-meta synth-node
                  {:data (assoc data :index index)})))))))

(defn canon->visual-event
  [nome canon]
  (let [period (-> canon meta :period)]
    (println period)
    {:total-dur (-> (if period
                      period
                      (canon-dur canon (metro-bpm nome)))
                    (* 1000)
                    long)
     :start-time (long (o/metro-beat nome (nome)))
     :name (-> canon meta :name (or "unknown canon") str)}))

(defn sample-canon
  [state canon & {:keys [pan nome] :or {pan 0 nome (metronome 60)}}]
  {:pre [(validate ::gspecs/state state)]}
  (v/add-event! (canon->visual-event nome canon))
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

(comment
  (require '[visuals.durations :as v])

  #_(add-event! {:total-dur 10000 :start-time (now) :name (str "canon " (rand-int 20)) })
  (require '[time-time.converge :refer [canon-dur]])
  (comment

    (do

      (->> (converge {:name :bosque-1
                      :durs (->> [7 5 5 7 5 5]
                                 (repeat 2)
                                 flatten)
                      :tempos (->> [7 5] (repeat 10) flatten)
                      :cps [10]
                      :period (* 2 60)
                      :bpm 60})
           (canon->visual-event (metronome 120))
           ;; v/add-event!
           ))

    (v/start-event-durations-gui)
    (double (o/metro-beat n (n)))
    (o/now)))
