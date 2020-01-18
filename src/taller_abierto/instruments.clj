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

(defonce orbitales
  (load-sample*
   "music/taller-abierto/instrumentos-1/renders/orbitales.wav"))

(defonce rebotes
  ;; "A diferentes escalas temporales"
  (load-sample*
   "music/taller-abierto/instrumentos-1/renders/rebotes-a-differentes-escalas-temporales.wav"))

(defn i-milo
  [name]
  (load-sample*
   (str
    "music/taller-abierto/instrumentos-milo-1/"
    name)))

(defonce orb1 (load-sample* "music/taller-abierto/sc/orbitales-canon-v1.2.wav"))
(defonce orb2 (load-sample* "music/taller-abierto/sc/orbitales-canon-v2.palecs-2.wav"))

;; (defonce silence (o/freesound-sample 459659))
(defonce a1 (i-milo "1.aiff"))
(defonce a2 (i-milo "2.aiff"))
(defonce a3 (i-milo "3.aiff"))
(defonce a4 (i-milo "4.aiff"))
(defonce a5 (i-milo "5.aiff"))
(defonce a6 (i-milo "6.aiff"))
(defonce a7 (i-milo "7.aiff"))
(defonce a8 (i-milo "8.aiff"))
(defonce a9 (i-milo "9.aiff"))
(defonce a10 (i-milo "10.aiff"))
(defonce amix (i-milo "mix_1.aiff"))

(comment
  (defonce fuego-atardecer (load-sample* "/music/despertar__en-un-sitio-sagrado-/2018.11.13-atardecer/renders/fuego-atardecer.wav"))
  (demo 30 ay-buf:ar 2 fuego-atardecer :start-pos 20000))

(defonce enlaces-organometalicos (i-milo "2/EnlacesOrganometalicos.wav"))
(defonce enlaces-organometalicos-2 (i-milo "2/EnlacesOrganometalicos-2.wav"))
(defonce escision (i-milo "2/Escision.wav"))
(defonce interior-hidro-statica-dinamica (i-milo "2/InteriorHidrostaticaHidrodinamica.wav"))
(defonce plectrum-interior (i-milo "2/PlectrumInterior.wav"))
(defonce refraccion-difraccion (i-milo "2/RefraccionDifraccion.wav"))
