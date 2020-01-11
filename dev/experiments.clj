(ns experiments)
(comment (require '[overtone.core :refer :all]
                  #_[nanc-in-a-can.taller-abierto.instruments :as i]))
(comment
  (boot-server)
  (kill-server)
  (def kick (freesound-sample 2086))
  (demo (sin-osc))
  (def hh (sample (freesound-path 44937)))
  (def grey-whale (sample (freesound-path 413377)))
  (def whales-1 (freesound-sample 322539))
  (def nome (metronome 200))
  (definst kick* [] (pan2 (play-buf:ar 1 kick :rate 0.7)))
  (defsynth whales-1* [pan 0] (out 0 (pan2 (play-buf:ar 1 whales-1) pan)))
  (kick*)
  (def a (kick))
  (node-active? a)
  (def ct #(ctl a :pan 1))
  (with-inactive-node-modification-error :warning (doseq [i (range 10)] (ct)))
  (def w1 (whales-1*))
  (ctl w1 :pan 1)
  (ctl w1 :pan -1)
  (ctl w1 :pan 0)
  (stop)
  (node-pause* w1)
  (node-start* w2))


(comment
  (require '[time-time.sequencing :refer [sequencer]])
  (def kick (freesound 2086))
  (let [nome (metronome 120)]
    (->> (converge {:durs (repeat 10 1)
                    :tempos [7 5]
                    :cps [5]
                    :bpm 120
                    :period 7})
         (map (fn [voice] (sequencer
                          nome
                          voice
                          (fn [vals index]
                            (kick)
                            nil)
                          {:repeat nil}))))))
