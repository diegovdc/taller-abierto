(ns taller-abierto.graphs.orbitales-2019-01-05
  "El sample original puede ser `fuego`"
  (:require [overtone.core :refer :all]
            [taller-abierto.instruments :as i]
            [taller-abierto.synths.sample-players :refer [m-distort]]
            [time-time.converge :refer [converge]]
            [time-time.standard :refer [->xos dur->sec]]
            [taller-abierto.standard :refer [*out-channels* ch mirror]]))

(def m-rand (memoize (fn [_] (rand))))
(def m-rand2 (memoize rand))

(defsynth m-distort2
  "Mono distorted synth that plays thin spectra slices"
  [sample i/silence
   a 3
   r 3
   dur 0
   pan 0
   amp 0.8
   start-pos 0
   rate 1
   bp-freq 1000
   bp-q 1
   out* 0]
  (let [env (envelope [0 0.7 0.1 0] [a dur r 3] :lin)]
    (out out* (distort
               (distort
                (* amp ;; BEWARE!!!!!!!!!!!!!!!!!!!!!
                   (env-gen env :action FREE)
                   (pan-az:ar *out-channels*
                              (bpf:ar
                               (play-buf:ar 1 sample
                                            :rate rate
                                            :start-pos (min 0 (- (rand 1000) start-pos))
                                            :loop (if (= sample i/silence)
                                                    false
                                                    true))
                               bp-freq
                               bp-q)
                              pan)))))))

(defn synth*
  [& {:keys [data metronome index start-pos sample pan amp]}]
  (println "orbs" index)
  (m-distort sample
             :amp 40
             :dur (* (rand 1) (dur->sec (:dur data) (metro-bpm metronome)))
             :rate (rand-nth
                    [1
                     #_(+ 0.8
                        (* 0.2
                           (rand-nth [1 -1])
                           (rand)
                           (m-rand2 (:tempo-index data))))])
             :a (* 0.05 (rand 0.51))
             :r (rand-nth [(* 0.2 (rand 5))
                           #_(rand 10)])
             :bp-freq (+ 10 (rand-int 600))
             :bp-q (max 0.07 (rand 0.51))
             :start-pos start-pos
             :pan (-> (ch) vals rand-nth)))
(comment
  (require '[taller-abierto.sample-canon :refer [sample-canon]]
           '[taller-abierto.graphs.logic.core :as g])
  (-> (g/play-next! state graph) :history last)
  (-> @state :history last)
  (def xos (->xos "xoooooxxooooxooooooooo"))
  (def xos (->xos "xooooo"))
  (def orbitales (sample-canon state canon))
  (stop))
(defn synth2
  [& {:keys [vals metronome index start-pos sample pan amp]}]
  (m-distort2 sample
              :amp 25
              :dur (* 0.001 (dur->sec (:dur vals) (metro-bpm metronome)))
              :rate (rand-nth
                     [1
                      (+ 0.8
                         (* 0.2
                            (rand-nth [1 -1])
                            (m-rand2 (:tempo-index vals))))])
              :a (* 0.5 (rand 1))
              :r (* 2 (rand 0.5))
              :bp-freq (+ 20 (rand-int 600))
              :bp-q (max 0.1 (rand 0.5))
              :start-pos start-pos
              :pan (-> (ch) clojure.core/vals rand-nth)))

(def profundo {:instruments [i/interior-hidro-statica-dinamica]
               :synth #'synth*})

(def circular {:instruments [i/refraccion-difraccion]
               :synth #'synth*})

(def circular-escision {:instruments [i/escision i/refraccion-difraccion]
                        :synth #'synth*})

(def escision {:instruments [i/escision]
               :synth #'synth*})

(def plectrum
  "agudos"
  {:instruments [i/plectrum-interior]
   :synth #'synth*})

(def enlaces-organometalicos
  "medios"
  {:instruments [i/enlaces-organometalicos
                 i/enlaces-organometalicos-2]
   :synth #'synth2})

(def graph {#'profundo #{#'circular}
            #'circular #{#'circular-escision #'profundo #'escision}
            #'circular-escision #{#'circular
                                  #'escision
                                  #'plectrum
                                  #'enlaces-organometalicos}
            #'escision #{#'enlaces-organometalicos}
            #'plectrum #{#'circular #'profundo}
            #'enlaces-organometalicos #{#'profundo}})

(def xos (->xos "x"))


(defonce state (atom {:history [] :xos #'xos}))

(def canon (converge {:durs (shuffle (flatten (repeat 150 [1 1/4 1/5 1/7])))
                      :tempos (range 7 10)
                      :cps [150 300 350 352]
                      :period (* 10 60)
                      :bpm 60}))


(comment
  (require '[taller-abierto.sample-canon :refer [sample-canon]]
           '[taller-abierto.graphs.logic.core :as g])
  (-> @state :history last)
  (def xos (->xos "xoooooooooooxxoooooooooxooooooooooooooo"))
  (def xos (->xos "x"))
  (-> (g/play-next! state graph) :history last)
  (def orbitales (sample-canon state canon))
  (stop))
