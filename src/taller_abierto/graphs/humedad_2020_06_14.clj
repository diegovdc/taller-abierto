(ns taller-abierto.graphs.humedad-2020-06-14
  (:require [overtone.core :as o]
            [taller-abierto.fader :refer [make-fader]]
            [taller-abierto.instruments :as i]
            [taller-abierto.sample-canon :refer [sample-canon]]
            [taller-abierto.standard :refer [*out-channels* ch wrap-at]]
            [taoensso.timbre :as log]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos]]
            [clojure.core.async :as a]))

(o/defsynth master
  [in 99 amp 1 fade-time 1]
  (o/out:ar 0
            (-> (o/in:ar in 2)
                (* amp))))

(defonce master*
  (let [node (master [:tail 0] :amp 0)]
    (with-meta (make-fader 2000 100 node :amp 0)
      {:node node})))


(defn make-sequencer [f-var sequence-var]
  (let [ctl (a/chan)]
    (a/go-loop [i 0]
      (let [dur (->> sequence-var var-get (wrap-at i))]
        (a/alt!
          (a/timeout (* 1000 dur))
          (do
            ((var-get f-var) dur i)
            (recur (inc i)))
          ctl (prn "Stopping..."))))
    #(a/>!! ctl true)))

(def s1 (interleave (map #(* 1 %) [1 2 1 1 4 5 2 1 1])
                    (map #(* 1.5 %) [7 7 5 6 7 3 4 7])))

(defn control-fader [dur index]
  (let [odd? (not= 0 (mod index 2))]
    (if (user/spy :odd? odd?)
      (master* (+ 0.5 (rand 1.5)))
      (master* (rand 0.3) 3000))))

(comment
  (def stop-sequencer (make-sequencer #'control-fader #'s1))
  (stop-sequencer) ; stops the sequencer
  )

(comment
  (def humedad (sample-canon state (canons 2)))
  (o/defsynth s2 [out 99 amp 1] (o/out:ar out (* 0.2 amp (o/sin-osc))))
  (s2 0 1)
  (def s (s2 99 0.1))
  (master* 1) ;; fader
  (o/stop)
  (o/kill s)
  (o/kill (-> master* meta :node))
  (o/kill-server))

(o/defsynth gas->crystal
  [sample i/silence
   start-pos 0
   rate 1
   amp 1.2
   dur 20
   bpf 500
   rq 1
   room 1
   pan 1
   out 0]
  (let [env (o/env-gen
             (o/envelope
              [0 0.7 0.5 1 0.3 0]
              [2 (/ dur 3) (/ dur 3) (/ dur 3) 4]
              :lin)
             :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 2 sig :start-pos start-pos :rate rate)
      (o/bpf sig bpf rq)
      (o/free-verb sig 1 room)
      #_(o/pan-az:ar *out-channels* sig pan)
      (* sig env amp)
      (o/out out sig))))

(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  (try
    (let [growth (mod index 20)]
      (when (> (/ growth 21) (rand))
        (log/info "humedad" (str "v" (:tempo-index data)) (str "i:" index))
        (gas->crystal sample
                      :amp (* 0.3 (+ 0.3 (rand)))
                      :room (* 0.5 growth)
                      :start-pos start-pos
                      :bpf (+ 50 (rand 6000))
                      :rq (+ 0.1 (rand 1.5))
                      :dur (* 1/10 (:dur data) growth)
                      :rate (+ 1 (rand (/ growth 8)))
                      :pan (-> (ch) vals rand-nth)
                      :out 99)))
    (catch Exception e (println e))))

(def vision-total {:instruments [i/humedad-1
                                 #_ i/humedad-2
                                 #_ i/humedad-3
                                 #_ i/humedad-4
                                 #_ i/humedad-5
                                   i/humedad-6]
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
             2 (converge {:durs (->> [9 5 5 3 7 5 3]
                                     (repeat 20)
                                     flatten)
                          :tempos (->> (range 11 17))
                          :cps [10 30 66 89 110 139]
                          :period (* 15 60)
                          :bpm 60})})

(comment
  (def humedad (sample-canon state (canons 2)))
  (def amp* 2)
  (user/rec "humedad_2020_06_16")
  (user/stop-rec)
  (o/stop))
