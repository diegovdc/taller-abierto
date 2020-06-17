(ns taller-abierto.graphs.viento-2020-02-06
  "this is actually a good wind"
  (:require [lorentz :as l]
            [overtone.core :as o]
            [scratch :refer [flocking]]
            [taller-abierto.instruments :as i]
            [taller-abierto.sample-canon :refer [ctl-list sample-canon]]
            [taller-abierto.standard :refer [*out-channels*
                                             +-
                                             mirror
                                             x-rand
                                             param
                                             node-name]]
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
  (let [env (o/env-gen (o/envelope [0 0.7 0.6 0] [5 dur 2] :lin)
                       :action o/FREE)]
    (as-> sample sig
      (o/play-buf:ar 1 sig :start-pos start-pos :rate rate)
      (o/pan-az:ar *out-channels* sig pan)
      (o/bpf:ar sig (o/env-gen (o/envelope [bpf-start bpf-end] [(+ 3 dur)])) 0.7)
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

;; viento alto 170-580 hz
;; viento bosque 8000-8500hz
(defn synth*
  [& {:keys [data index start-pos sample pan amp state]}]
  (let [base-amp (param state :base-amp 0.3)
        low-band (param state :low-band 170)
        density (param state :density 1)
        high-band (param state :high-band 380)
        offset (* 5 (:tempo-index data))
        {{:keys [rate pan amp bpf-start bpf-end start-pos]} :params}
        (flocking {:flocking/index-mode :total
                   :rate {:init 1 :f (l-coord :x 0.5 1.2 offset)}
                   :pan {:init 1 :f (l-coord :x -0.25 1.5 (data :tempo-index))}
                   :amp {:init 0.01 :f (l-coord :y 0 1 offset)}
                   :bpf-start {:init 20 :f (l-coord :z low-band high-band offset)}
                   :bpf-end {:init 20
                             :f (l-coord :z low-band high-band (+ 100 offset))}
                   :start-pos {:init 20
                               :f (l-coord :z 0 (-> sample :n-samples) (inc offset))}}
                  flock-state
                  index)]
    (when (>= density (rand))
      (println "viento:" (node-name state))
      (gas->crystal sample
                    :amp (* base-amp amp)
                    :start-pos (int start-pos)
                    :dur (/ (:dur data) 2)
                    :bpf-end bpf-end
                    :bpf-start bpf-start
                    :pan pan
                    :rate rate))))


(def params** (atom {:low-band 100 :high-band 8000 :base-amp 1 :density 0.3}))
(def dentro-del-bosque {:instruments [i/rebotes i/refraccion-difraccion]
                        :synth #'synth*
                        :params #'params**})

(def params* (atom {:low-band 270 :high-band 380 :base-amp 0.1 :density 0.7}))
(def sobre-el-bosque {:instruments [i/rebotes i/orb1]
                      :synth #'synth*
                      :params #'params*})

(def graph {#'dentro-del-bosque #{#'sobre-el-bosque}
            #'sobre-el-bosque #{#'dentro-del-bosque}})

(declare xos)
(defonce state (atom {:history [] :xos #'xos}))
(swap! state assoc :history [#'sobre-el-bosque])
(comment (swap! state assoc :voicef (set (range 10 20)))
         (swap! state assoc :voicef true))

(def canons {1 (converge {:durs (repeat 2500 2)
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
  (require '[taller-abierto.graphs.logic.core :as g])
  (do (g/play-next! state graph) :true)
  (o/stop)
  (def xos (shuffle (concat (repeat 100 true) (repeat 150 false))))
  (def viento (sample-canon state (canons 1)))
  (meta (canons 2)))

(comment (ctl-list state #(o/ctl % :pan 1)))
