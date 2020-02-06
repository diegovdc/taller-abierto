(ns taller-abierto.graphs.viento
  (:require [overtone.core :as o]
            [taller-abierto.sample-canon :refer [sample-canon ctl-list]]
            [taller-abierto.instruments :as i]
            [taller-abierto.standard :refer [*out-channels* mirror +-]]
            [time-time.converge :refer [converge]]
            [scratch :refer [flocking]]
            [lorentz :as l]
            [time-time.standard :refer [->xos dur->sec]]))

(o/defsynth gas->crystal
  [sample i/a1
   amp 1
   start-pos 0
   rate 1
   dur 20
   pan 1
   depth 0.8
   bpf-start 10000
   bpf-end 10000]
  (let [env (o/env-gen (o/envelope [0 1 1 0] [20 dur 10] :lin) :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig :start-pos start-pos :rate rate)
      (o/pan-az:ar *out-channels* sig pan)
      (o/bpf:ar sig (o/env-gen (o/envelope [bpf-start bpf-end] [(+ 14 dur)])) 0.5)
      (o/free-verb:ar sig depth 0.51 0.2)
      (* sig env amp)
      #_(o/distort sig)
      (o/distort sig)
      (o/out 0 sig))))


(def lor (l/init-system))
(defn l-coord
  ([coord min max] (l-coord coord min max 0))
  ([coord min max offset]
   (fn ([i _] (-> i (+ offset) lor (l/bound coord min max))))))
(def flock-state (atom nil))


(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  (println  index)
  (let [offset 1 #_(* 40 (:tempo-index data))
        {:keys [params]}
        (flocking {:flocking/index-mode :total
                   :rate {:init 1 :f (l-coord :x 0.7 1.8 offset)}
                   :amp {:init 0.5 :f (l-coord :y 0.3 1.5 offset)}
                   :bpf-start {:init 0.5 :f (l-coord :z 100 150 offset)}
                   :bpf-end {:init 0.5 :f (l-coord :z 100 15000 (+ 100 offset))}}
                  flock-state
                  index)]
    (gas->crystal sample
                  :amp (* 2 (params :amp))
                  :start-pos start-pos
                  :dur (:dur data)
                  :bpf-end (params :bpf-end)
                  :bpf-start (params :bpf-start)
                  :pan (rand 2)
                  :rate (:rate params))))

(def vision-total {:instruments [i/fuego-atardecer]
                   :synth #'synth*})


(def graph {#'vision-total #{#'vision-total}})

(declare xos)
(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'vision-total])
(comment (swap! state assoc :voicef (set (range 10 20)))
         (swap! state assoc :voicef true))

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
  (def xos (->xos "xooxxooo"))
  (def viento (sample-canon state (canons 3)))
  (meta (canons 2)))

(comment (ctl-list state #(o/ctl % :pan 1)))
