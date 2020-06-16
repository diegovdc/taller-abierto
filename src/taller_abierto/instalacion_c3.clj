(ns taller-abierto.instalacion-c3
  (:require [taller-abierto.main-sequencer :as ms]
            [taller-abierto.instruments :as i]
            [taller-abierto.graphs.bosque-nocturno-2019-11-01
             :as bosque-noctruno]
            [taller-abierto.play-static-samples :refer [play-static-samples]]))

(def global-state (atom {:bosque-nocturno bosque-noctruno/state}))

(def instrumento-2
  {i/m2-1 {:out 0 :amp 1.5}
   i/m2-2 {:out 1 :amp 1.5}
   i/m2-3 {:out 2 :amp 1.5}
   i/m2-4 {:out 3 :amp 1.5}})

(play-static-samples instrumento-2)

(def secuencia
  [{:at 0 :synth #(play-static-samples instrumento-2)}
   {:at 5 :synth #(bosque-noctruno/play! 1)}])

(ms/play! secuencia)
