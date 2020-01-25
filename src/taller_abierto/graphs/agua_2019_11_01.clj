(ns taller-abierto.graphs.agua-2019-11-01
  (:require [overtone.core :as o]
            [taller-abierto.sample-canon :refer [sample-canon ctl-list]]
            [taller-abierto.instruments :as i]
            [taller-abierto.standard :refer [*out-channels*]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos dur->sec]]))

(o/defsynth gas->crystal
  [sample i/silence
   amp 1
   start-pos 0
   rate 1
   dur 20
   pan 1]
  (let [env (o/env-gen
             (o/envelope
              [0 1 0]
              [20 dur 20]
              :lin)
             :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig
                     :start-pos start-pos
                     :rate rate)
      (o/pan-az:ar *out-channels* sig pan)
      (* sig env amp)
      (o/out 0 sig))))

(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  #_(println data start-pos )
  (gas->crystal sample
                :amp 1.2
                :start-pos start-pos
                :dur (:dur data)
                :rate (+ 0.6 (rand))))

(def vision-total {:instruments [i/a1]
                   :synth #'synth*})

(def graph {#'vision-total #{#'vision-total}})

(declare xos)
(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'vision-total])
(comment (swap! state assoc :voicef #{1}))

(defn mirror [xs] (concat xs (reverse xs)))

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
             3 (converge {:durs (->> [7 5 5 7 5 5]
                                     (repeat 20)
                                     (map-indexed (fn [i v] (map #(+ % i) v)))
                                     flatten
                                     mirror)
                          :tempos (->> [7 5] (repeat 20) flatten)
                          :cps [10 20 80 110 120 121 170 172 174 177 178]
                          :period (* 20 60)
                          :bpm 60})})

(comment
  (require '[taller-abierto.graphs.specs :as gspecs]
           '[clojure.spec.alpha :as s])
  #_(s/explain ::gspecs/node* vision-total)
  (g/play-next! state graph)
  (o/stop)
  (def xos (->xos "x"))
  (def viento (sample-canon state (canons 3)))
  (meta (canons 2)))

(comment (ctl-list state #(o/ctl % :pan 1)))
