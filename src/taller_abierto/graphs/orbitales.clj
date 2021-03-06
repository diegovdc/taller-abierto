(ns taller-abierto.graphs.orbitales
  "El sample original puede ser `fuego`"
  (:require [overtone.core :refer [metro-bpm]]
            [taller-abierto.instruments :as i]
            [taller-abierto.synths.sample-players :refer [m-distort]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos dur->sec]]))

(def m-rand (memoize (fn [_] (rand))))
(def m-rand2 (memoize rand))

(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  (m-distort sample
             :amp 15
             :dur (* 0.1 (dur->sec (:dur data) (metro-bpm metronome)))
             :rate (rand-nth
                    [1
                     (+ 0.8
                        (* 0.2
                           (rand-nth [1 -1])
                           (m-rand2 (:tempo-index data))))])
             :a (+ 0 (rand 2))
             :r (+ 0.7 (rand 0.5))
             :bp-freq (+ 20 (rand-int 1600))
             :bp-q (max 0.1 (rand 0.5))
             :start-pos start-pos
             :pan pan))

(def node-a {:instruments [i/orbitales]
             :synth #'synth*})

(def node-b {:instruments [i/rebotes]
             :synth #'synth*})

(def node-c {:instruments [i/a1 i/a2]
             :synth #'synth*})

(def graph {#'node-a #{#'node-b #'node-c}
            #'node-b #{#'node-a}
            #'node-c #{#'node-b}})

(def xos (->xos "x"))

(defonce state (atom {:history [] :xos #'xos}))

(def canon (converge {:durs (flatten (repeat 300 [1/4 1]))
                      :tempos (map #(/ % 7) (range 7 15))
                      :cps [150]}))
(comment
  (require '[taller-abierto.sample-canon :refer [sample-canon]]
           '[taller-abierto.graphs.logic.core :as g])
  (g/play-next! state graph)
    (require '[taller-abierto.graphs.specs :as gspecs]
             '[clojure.spec.alpha :as s])
  (def orbitales (sample-canon state canon))
  (stop))
