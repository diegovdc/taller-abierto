(ns taller-abierto.graphs.bosque-nocturno-2019-11-01
  (:require [overtone.core :as o :refer :all]
            [taller-abierto.instruments :as i]
            [taller-abierto.sample-canon :refer [ctl-list sample-canon]]
            [taller-abierto.standard :refer [*out-channels* rrange +- ch]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos]]))

(o/defsynth gas->crystal
  [sample i/a1
   amp 1
   start-pos 0
   rate 1
   dur 20
   pan 1
   depth 0.2
   bpf-start 10000
   bpf-end 10000]
  (let [env (o/env-gen (o/envelope [0 1 1 0] [1 dur 1] :lin) :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig :start-pos start-pos :rate rate)
      (o/pan-az:ar *out-channels* sig pan)
      (o/bpf:ar sig (o/env-gen (o/envelope [bpf-start bpf-end] [dur])) 0.1)
      (o/free-verb:ar sig depth 0.5 0.3)
      (* sig env amp)
      (o/distort sig)
      (o/distort sig)
      (o/out 0 sig))))

(gas->crystal i/a1 :bpf-start 200 :bpf-end 300 :start-at 88000 :amp 10)
(o/stop)
(defn synth*
  [& {:keys [vals metronome index start-pos sample pan amp]}]
  (println index )
  (let [r1 (rrange 500 7000)
        r2 (rrange 2000 10000)]
    (gas->crystal sample
                  :amp 30
                  :depth (rand)
                  :bpf-start (rand-nth [r1 r2])
                  :bpf-end (rand-nth [r1 r2])
                  :start-pos start-pos
                  :dur (:dur vals)
                  :rate (+- 1 (rand))
                  :pan (-> (ch) clojure.core/vals rand-nth))))

(def insectos
  "usar :voicef"
  {:instruments [i/a1 i/fuego-atardecer]
   :synth #'synth*})

(def graph {#'insectos #{#'insectos}})


(def xos (->xos "x"))
(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'insectos])
(comment (swap! state assoc :voicef #{7 8 9 10 12})
         (swap! state assoc :voicef true))

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
                          :tempos (->> [70 50]
                                       (repeat 21)
                                       flatten
                                       (map
                                        #(+- % (rand-nth [1 2 3 10 30]))))
                          :cps [10 20 80 110 120 121 170 172 174 177 178]
                          :period (* 20 60)
                          :bpm 60})})

(comment
  (g/play-next! state graph)
  (o/stop)
  (def xos (->xos "x"))
  (def bosque-nocturno (sample-canon state (canons 3)))
  (meta (canons 2)))

(comment (ctl-list state #(o/ctl % :pan 1)))




(defsynth sample-on-channel [sample i/a1 ch 0 amp 1 start-at 0]
  (out ch (* amp (play-buf:ar 1 sample :start-pos start-at))))

(comment
  (do
    (def milo
      [(sample-on-channel i/m2-1 4 1.5 :start-at (* 44100 60 8))
       (sample-on-channel i/m2-2 5 1.5 :start-at (* 44100 60 5))
       (sample-on-channel i/m2-3 6 1.5 :start-at (* 44100 60 3))
       (sample-on-channel i/m2-4 7 1.5 :start-at (* 44100 60 8))])))
