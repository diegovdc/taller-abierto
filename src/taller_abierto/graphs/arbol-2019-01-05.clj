(ns taller-abierto.graphs.viento
  (:require [overtone.core :as o]
            [taller-abierto.sample-canon :refer [sample-canon]]
            [taller-abierto.instruments :as i]
            [taller-abierto.standard :refer [*out-channels*]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos dur->sec]]))

(o/defsynth gas->crystal
  [sample i/silence
   start-pos 0
   rate 1
   dur 20
   pan 1]
  (let [env (o/env-gen
             (o/envelope
              [0 1 0.1 0]
              [20 dur 20]
              :lin)
             :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig
                     :start-pos start-pos
                     :rate rate)
      (o/pan-az:ar *out-channels* sig pan)
      (* sig env 1.2)
      (o/out 0 sig))))

(defn synth*
  [& {:keys [vals metronome index start-pos sample pan amp]}]
  (println vals start-pos)
  (gas->crystal sample
                :start-pos start-pos
                :dur (:dur vals)
                :rate (+ 0.6 (rand))))

(def vision-total {:instruments [i/fuego-atardecer]
                   :synth #'synth*})

(def graph {#'vision-total #{#'vision-total}})

(def xos (->xos "x"))

(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'vision-total])


(defn mirror
  [xs]
  (concat xs (reverse xs)))

(def canons {1 (converge {:durs (->> [7 5 5 7 5 5]
                                     (repeat 2)
                                     flatten)
                          :tempos (->> [7 5] (repeat 10) flatten)
                          :cps [10]
                          :period (* 5 60)
                          :bpm 60})
             2  (converge {:durs (->> [7 5 5 7 5 5]
                                      (repeat 2)
                                      (map-indexed (fn [i v] (map #(+ % i) v)))
                                      flatten
                                      mirror)
                           :tempos (->> [7 5] (repeat 10) flatten)
                           :cps [10 20]
                           :period (* 10 60)
                           :bpm 60})
             :arbol (converge {:durs (->> [7 5 5 7 5 5]
                                     (repeat 20)
                                     (map-indexed (fn [i v] (map #(+ % i) v)))
                                     flatten
                                     mirror)
                          :tempos (->> [7 5] (repeat 20) flatten)
                          :cps [10 20 80 110 120 121 170 172 174 177 178]
                          :period (* 20 60)
                          :bpm 60})})

(comment
  (g/play-next! state graph)
  (o/stop)
  (def viento (sample-canon state (canons :arbol)))
  (o/recording-start "~/Desktop/viento-autonomo.wav")
  (o/recording-stop))
(meta (canons 2))

(comment
  (require '[time-time.sequencing :refer [sequencer]])
  (def kick (o/freesound 2086))
  (let [nome (o/metronome 120)]
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
