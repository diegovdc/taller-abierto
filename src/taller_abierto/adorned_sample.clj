(ns taller-abierto.adorned-sample
  (:require [overtone.core :as o]
            [taller-abierto.sample-canon :refer [sample-canon]]
            [taller-abierto.synths.sample-players :refer [sbase]]
            [time-time.converge :refer [converge]]))

(defn adorned-sample
  [state base-sample canon-config chan]
  (let [period (:duration base-sample)
        bpm 60
        canon (converge (merge
                         canon-config
                         {:period period :bpm bpm}))]
    {:sbase  (sbase base-sample (:n-channels base-sample) chan)
     :canon (sample-canon (o/metronome bpm)
                          state
                          canon
                          :pan chan)}))
