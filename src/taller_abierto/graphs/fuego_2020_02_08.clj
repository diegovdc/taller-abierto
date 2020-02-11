(ns taller-abierto.graphs.fuego-2020-02-08
  "NOTE: renderear unas versiones en reaper"
  (:require [overtone.core :as o]
            [taller-abierto.instruments :as i]
            [taller-abierto.sample-canon :refer [sample-canon]]
            [taller-abierto.standard :refer [*out-channels* ch mirror]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos]]))

(o/defsynth gas->crystal
  [sample i/silence
   start-pos 0
   rate 1
   amp 1.2
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
      (o/free-verb sig 0.5 0.5)
      (o/pan-az:ar *out-channels* sig pan)
      (* sig env amp)
      (o/out 0 sig))))

(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  (println "arbol" index)
  (when (> 0.0 (rand))
    (gas->crystal sample
                  :amp (rand)
                  :start-pos start-pos
                  :dur (:dur data)
                  :rate (+ 0.6 (rand))
                  :pan (-> (ch) vals rand-nth))))

(def vision-total {:instruments [i/fa-1 i/fa-2 i/fa-3 i/fa-4]
                   :synth #'synth*})
(def f2 {:instruments [i/fa-2]
                   :synth #'synth*})

(def graph {#'vision-total #{#'vision-total #'f2}
            #'f2 #{#'vision-total}})

(def xos (->xos "x"))

(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'vision-total])

(def canons {1 (converge {:durs (->> [7 5 5 7 5 5]
                                     (repeat 2)
                                     flatten)
                          :tempos (->> (range 11 20))
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
  (alter-var-root #'*out-channels* (constantly 4))
  (identity *out-channels*)
  (require '[taller-abierto.graphs.logic.core :as g])
  (do (g/play-next! state graph) true)
  (o/stop)
  (def viento (sample-canon state (canons :arbol))) ;;
  (o/recording-stop)
  (-> *out-channels*)
  (-> (ch))
  (meta (canons 2)))

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
