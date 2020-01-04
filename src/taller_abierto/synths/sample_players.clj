(ns taller-abierto.synths.sample-players
 (:require [overtone.core :as o :refer :all]
           [taller-abierto.instruments :as i]
           [taller-abierto.standard :refer [*out-channels*]]))

(defsynth m-distort
  "Mono distorted synth that plays thin spectra slices"
  [sample i/silence
   a 3
   r 3
   dur 0
   pan 0
   amp 0.8
   start-pos 0
   rate 1
   bp-freq 1000
   bp-q 1
   out* 0]
  (let [env (envelope [0 0.7 0.1 0] [a dur r 3] :lin)]
    (out out* (distort
               (distort
                (* amp ;; BEWARE!!!!!!!!!!!!!!!!!!!!!
                   (env-gen env :action FREE)
                   (pan2 (bpf:ar
                          (play-buf:ar 1 sample
                                       :rate rate
                                       :start-pos (min 0 (- (rand 1000) start-pos))
                                       :loop (if (= sample i/silence)
                                               false
                                               true))
                          bp-freq
                          bp-q)
                         pan)))))))

(defsynth sbase
  "Mono sample player"
  [sample i/silence
   pan 1]
  (as-> sample sig
    (play-buf:ar 1 sig)
    (pan-az:ar *out-channels* sig pan)
    (out 0 sig)))
