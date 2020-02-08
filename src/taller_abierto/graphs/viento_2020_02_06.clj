(ns taller-abierto.graphs.viento-2020-02-06
  "this is actually a good wind"
  (:require [lorentz :as l]
            [overtone.core :as o]
            [scratch :refer [flocking]]
            [taller-abierto.instruments :as i]
            [taller-abierto.sample-canon :refer [ctl-list sample-canon]]
            [taller-abierto.standard :refer [*out-channels* +- mirror x-rand]]
            [time-time.converge :refer [converge]]))

(o/defsynth gas->crystal
  [sample i/a1
   amp 1
   start-pos 0
   rate 1
   dur 20
   pan 1
   mod- 1
   depth 0.8
   bpf-start 10000
   bpf-end 10000]
  (let [env (o/env-gen (o/envelope [0 0.7 0.6 0] [2 dur 2] :lin)
                       :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig :start-pos start-pos :rate rate)
      (o/pan-az:ar *out-channels* sig pan)
      (o/bpf:ar sig (o/env-gen (o/envelope [bpf-start bpf-end] [(+ 3 dur)])) 0.7)
      #_(o/free-verb:ar sig depth 0.51 0.2)
      (* sig env amp (o/brown-noise))
      #_(o/distort sig)
      (o/distort sig)
      (o/out 0 sig))))


(def lor (l/init-system))
(defn l-coord
  ([coord min max] (l-coord coord min max 0))
  ([coord min max offset]
   (fn ([i _] (-> i (+ offset) lor (l/bound coord min max))))))
(def flock-state (atom nil))

;; viento alto 170-580 hz
;; viento bosque 8000-8500hz
(defn synth*
  [& {:keys [data index start-pos sample pan amp]}]
  (let [offset (* 5 (:tempo-index data))
        {:keys [params]}
        (flocking {:flocking/index-mode :total
                   :rate {:init 1 :f (l-coord :x 0.5 1.2 offset)}
                   :pan {:init 1 :f (l-coord :x 0.5 1.5 (data :tempo-index))}
                   :amp {:init 0.01 :f (l-coord :y 0 0.2 offset)}
                   :bpf-start {:init 20 :f (l-coord :z 170 580 offset)}
                   :bpf-end {:init 20 :f (l-coord :z 170 580 (+ 100 offset))}
                   :start-pos {:init 20 :f (l-coord :z 170 (-> sample :n-samples) (inc offset))}
                   :mod {:init 20 :f (l-coord :y -200 500 offset) }}
                  flock-state
                  index)]
    (gas->crystal sample
                  :mod- (max 0 (params :mod))
                  :amp (* 0.1 (params :amp))
                  :start-pos (int (params :start-pos))
                  :dur (/ (:dur data) 2)
                  :bpf-end (params :bpf-end)
                  :bpf-start (params :bpf-start)
                  :pan (:pan params)
                  :rate (:rate params))))
(type (->  i/a1 :n-samples))
(def vision-total {:instruments [i/fuego-atardecer]
                   :synth #'synth*})

(def particulas {:instruments [i/rebotes i/orb1]
                 :synth #'synth*})


(def graph {#'vision-total #{#'vision-total}
            #'particulas #{#'particulas}})

(declare xos)
(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'particulas])
(comment (swap! state assoc :voicef (set (range 10 20)))
         (swap! state assoc :voicef true))

(def canons {1 (converge {:durs (->>
                                 (repeat 5000 2)
                                 flatten)
                          :tempos (->> (x-rand 10 10 30))
                          :period (* 20 60)
                          :cp (x-rand 5 20 5000)
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
             :grillos-intenso (converge {:durs (->> [7 5 3 13 12 9 1]
                                     (repeat 20)
                                     (map-indexed (fn [i v] (map #(+ % i) v)))
                                     flatten
                                     mirror)
                          :tempos (->> [7 5 9 13] (repeat 20) flatten
                                       (map #(+- % (rand-int 5))))
                          :cps [10 20 80 110 120 121 ]
                          :period (* 20 60)
                                         :bpm 60})})

(comment
  (g/play-next! state graph)
  (o/stop)
  (def xos (shuffle (concat (repeat 100 true) (repeat 500 false))))
  (def viento (sample-canon state (canons 1)))
  (meta (canons 2)))

(comment (ctl-list state #(o/ctl % :pan 1)))
