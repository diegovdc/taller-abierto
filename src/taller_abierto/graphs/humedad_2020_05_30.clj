(ns taller-abierto.graphs.humedad-2020-05-30
  (:require [overtone.core :as o]
            [taoensso.timbre :as log]
            [taller-abierto.instruments :as i]
            [taller-abierto.sample-canon :refer [sample-canon]]
            [taller-abierto.standard :refer [*out-channels* ch mirror +-]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos]]))



(o/defsynth gas->crystal
  [sample i/silence
   start-pos 0
   rate 1
   amp 1.2
   dur 20
   bpf 500
   rq 1
   room 1
   pan 1]
  (let [env (o/env-gen
             (o/envelope
              [0 0.7 0.5 1 0.1 0]
              [1 (/ dur 3)
               (/ dur 3)
               (/ dur 3)
               2]
              :lin)
             :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig
                     :start-pos start-pos
                     :rate rate)
      (o/bpf sig bpf rq)
      (o/free-verb sig 0.5 room)
      (o/pan-az:ar *out-channels* sig pan)
      (* sig env amp)
      (o/out 0 sig))))

(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  (try
    (let [growth (mod index 10)]
      (when (> (/ growth 20) (rand))
        (log/info "humedad" (str "v" (:tempo-index data)) (str "i:" index))
        (gas->crystal sample
                      :amp (* 0.3 (+ 0.3 (rand)))
                      :room (* 0.15 growth)
                      :start-pos start-pos
                      :bpf (+ 500 (rand 5000))
                      :rq (+ 0.1 (rand))
                      :dur (* 1 (:dur data) growth)
                      :rate (+ 0.9 (rand (/ growth 10)))
                      :pan (-> (ch) vals rand-nth))))
    (catch Exception e (println e))))


(def vision-total {:instruments [#_ i/humedad-1
                                 #_ i/humedad-2
                                 #_ i/humedad-3
                                  i/humedad-4
                                 #_ i/humedad-5
                                 #_  i/humedad-6]
                   :synth #'synth*})

(def graph {#'vision-total #{#'vision-total}})

(def xos (->xos "x"))

(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'vision-total])

(def canons {1 (converge {:durs (->> [7 5 5 7 5 5]
                                     (repeat 5)
                                     flatten)
                          :tempos (->> (range 11 20))
                          :cps [10 7]
                          :period (* 5 60)
                          :bpm 60})
             2 (converge {:durs (->> [7 5 5 3 7 5 3]
                                     (repeat 20)
                                     flatten)
                          :tempos (->> (range 11 27))
                          :cps [10 30 66 89 110 139]
                          :period (* 15 60)
                          :bpm 60})})


(comment
  (def humedad (sample-canon state (canons 2)))
  (def amp* 2)
  (user/rec "humedad_2020_05_30")
  (user/stop-rec)
  (o/stop))
