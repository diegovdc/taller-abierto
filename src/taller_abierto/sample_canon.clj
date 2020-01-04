(ns taller-abierto.sample-canon
  (:require
   [overtone.core :refer [metronome]]
   [taller-abierto.instruments :as i]
   [taller-abierto.standard :refer [get-instruments get-synth xo-play?]]
   [time-time.sequencing :refer [sequencer]]
   [time-time.standard :refer [nthw rand-pos]]))

(defn smpl-playa [vals index nome state sample-sequence pan]
  (let [at-idx (get @sample-sequence index)
        smpl (or (:smpl at-idx) (nthw (get-instruments state) index i/silence))
        start-pos (or (:start-pos at-idx) (rand-pos smpl))
        synth* (get-synth state)]
    (when (nil? at-idx)
      (swap! sample-sequence #(assoc % index {:start-pos start-pos
                                              :smpl smpl})))

    (when (xo-play? state index)
      (synth* :vals vals
              :metronome nome
              :index index
              :sample smpl
              :start-pos start-pos
              :pan pan))))

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
