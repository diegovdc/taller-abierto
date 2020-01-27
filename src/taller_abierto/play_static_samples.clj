(ns taller-abierto.play-static-samples
  (:require [overtone.core :as o :refer [defsynth disk-in:ar]]
            [taller-abierto.instruments :as i]
            [taller-abierto.graphs.specs :as specs]
            [taller-abierto.internal :refer [validate]]))

(defsynth sample-on-channel [sample i/silence out 0 amp 1]
  (o/out out (* amp (disk-in:ar 1 sample))))

(defn play-static-samples
  [static-samples-map]
  {:pre [(validate ::specs/static-samples-map static-samples-map)]}
  (->> static-samples-map
       (map (fn [[sample config]]
              (sample-on-channel
               sample
               (get config :out 0)
               (get config :amp 1))))))
