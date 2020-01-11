(ns taller-abierto.graphs.viento
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
  #_(println vals start-pos)
  (user/spy (gas->crystal sample
                          :start-pos start-pos
                          :dur (:dur vals)
                          :rate (+ 0.6 (rand))
                          :pan (-> (ch) clojure.core/vals rand-nth user/spy))))

(def vision-total {:instruments [i/fuego-atardecer]
                   :synth #'synth*})

(def graph {#'vision-total #{#'vision-total}})

(def xos (->xos "x"))

(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'vision-total])

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
  (require '[taller-abierto.graphs.logic.core :as g])
  (alter-var-root #'*out-channels* (constantly 4))
  (identity *out-channels*)
  (g/play-next! state graph)
  (o/stop)
  (def viento (sample-canon state (canons 1)))
  (o/recording-start "~/Desktop/viento-autonomo.wav")
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


(comment
  (def profundo
    "Una sola gota, dentro del agua"
    {:instruments [i/interior-hidro-statica-dinamica]
     :synth #'synth*})

  (def circular {:instruments [i/refraccion-difraccion]
                 :synth #'synth*})

  (def circular-escision {:instruments [i/escision i/refraccion-difraccion]
                          :synth #'synth*})

  (def escision {:instruments [i/escision]
                 :synth #'synth*})

  (def plectrum
    "agudos, grillos"
    {:instruments [i/plectrum-interior]
     :synth #'synth*})

  (def enlaces-organometalicos
    "medios"
    {:instruments [i/enlaces-organometalicos
                   i/enlaces-organometalicos-2]
     :synth #'synth*})

  (def graph-1 {#'profundo #{#'circular}
                #'circular #{#'circular-escision #'profundo #'escision}
                #'circular-escision #{#'circular
                                      #'escision
                                      #'plectrum
                                      #'enlaces-organometalicos}
                #'escision #{#'enlaces-organometalicos}
                #'plectrum #{#'circular #'profundo}
                #'enlaces-organometalicos #{#'profundo}})

  (def xos (->xos "xoooooooooooo"))


  (defonce state-1 (atom {:history [] :xos #'xos}))

  (g/play-next! state-1 graph-1)
  (o/stop)
  (def metal-tierra (sample-canon state-1 (canons 1))))
