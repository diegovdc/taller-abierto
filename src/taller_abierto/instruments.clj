(ns taller-abierto.instruments
  (:require [clojure.string :as string]
            [overtone.core :as o :refer :all]))

(def ^:dynamic *drives* {:linux "/media/diego/Music/"
                         :windows "F:\\"})
(defn load-sample* [path]
  (let [windows? (string/includes? (System/getProperty "os.name")
                                   "Windows")
        drive (if windows? (*drives* :windows) (*drives* :linux))
        path* (if windows? (string/replace (str drive path) #"/" "\\\\")
                  (str drive path))]
    (when-not (or windows? (o/server-connected?))
      (o/boot-external-server))
    (load-sample path*)))

(def orbitales
  (load-sample*
   "music/taller-abierto/instrumentos-1/renders/orbitales.wav"))

(def rebotes
  "A diferentes escalas temporales"
  (load-sample*
   "music/taller-abierto/instrumentos-1/renders/rebotes-a-differentes-escalas-temporales.wav"))

(defn i-milo
  [name]
  (load-sample*
   (str
    "music/taller-abierto/instrumentos-milo-1/"
    name)))

(def orb1 (load-sample* "music/taller-abierto/sc/orbitales-canon-v1.2.wav"))
(def orb2 (load-sample* "music/taller-abierto/sc/orbitales-canon-v2.palecs-2.wav"))

(def silence (o/freesound-sample 459659))
(def a1 (i-milo "1.aiff"))
(def a2 (i-milo "2.aiff"))
(def a3 (i-milo "3.aiff"))
(def a4 (i-milo "4.aiff"))
(def a5 (i-milo "5.aiff"))
(def a6 (i-milo "6.aiff"))
(def a7 (i-milo "7.aiff"))
(def a8 (i-milo "8.aiff"))
(def a9 (i-milo "9.aiff"))
(def a10 (i-milo "10.aiff"))
(def amix (i-milo "mix_1.aiff"))

(def fuego-atardecer (load-sample* "/music/despertar__en-un-sitio-sagrado-/2018.11.13-atardecer/renders/fuego-atardecer.wav"))
