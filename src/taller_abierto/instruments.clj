(ns taller-abierto.instruments
  (:require [clojure.string :as string]
            [user :refer [windows?]]
            [overtone.core :as o :refer :all]))

(def ^:dynamic *drives* {:linux "/media/diego/Music/"
                         :windows "F:\\"})
(defn load-sample* [path]
  (let [drive (if windows? (*drives* :windows) (*drives* :linux))
        path* (if windows? (string/replace (str drive path) #"/" "\\\\")
                  (str drive path))]
    (load-sample path*)))

(defn sample-path [path]
  (let [windows? (string/includes? (System/getProperty "os.name")
                                   "Windows")
        drive (if windows? (*drives* :windows) (*drives* :linux))
        path* (if windows? (string/replace (str drive path) #"/" "\\\\")
                  (str drive path))]
    path*))

(def orbitales
  (load-sample*
   "music/taller-abierto/instrumentos-1/renders/orbitales.wav"))

(def rebotes
  ;; "A diferentes escalas temporales"
  (load-sample*
   "music/taller-abierto/instrumentos-1/renders/rebotes-a-differentes-escalas-temporales.wav"))

(defn i-milo
  [name]
  (load-sample*
   (str
    "music/taller-abierto/instrumentos-milo-1/"
    name)))

(defn milo-path
  [path]
  (str
   "music/taller-abierto/instrumentos-milo-1/"
   path))

(def orb1 (load-sample* "music/taller-abierto/sc/orbitales-canon-v1.2.wav"))
(def orb2 (load-sample* "music/taller-abierto/sc/orbitales-canon-v2.palecs-2.wav"))

(def silence (load-sample "resources/silence.wav"))
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

(comment
  (def fuego-atardecer (load-sample* "/music/despertar__en-un-sitio-sagrado-/2018.11.13-atardecer/renders/fuego-atardecer.wav")))

(def enlaces-organometalicos (i-milo "2/EnlacesOrganometalicos.wav"))
(def enlaces-organometalicos-2 (i-milo "2/EnlacesOrganometalicos-2.wav"))
(def escision (i-milo "2/Escision.wav"))
(def interior-hidro-statica-dinamica (i-milo "2/InteriorHidrostaticaHidrodinamica.wav"))
(def plectrum-interior (i-milo "2/PlectrumInterior.wav"))
(def refraccion-difraccion (i-milo "2/RefraccionDifraccion.wav"))

(defn cue-milo
  [milo-path-file]
  (-> milo-path-file milo-path sample-path (buffer-cue :size 131072)))

(def m2-1 (-> "2-completo/1.wav" cue-milo))
(def m2-2 (-> "2-completo/2.wav" cue-milo))
(def m2-3 (-> "2-completo/3.wav" cue-milo))
(def m2-4 (-> "2-completo/4.wav" cue-milo))


(comment
  (require '[clojure.spec.alpha :as s])
  (s/def ::amp number?)
  (s/def ::out number?)
  (s/def ::sample buffer?)
  (s/def ::static-samples-map
    (s/map-of ::sample (s/keys :opt-un [::out ::amp])))

  (s/def ::ch number?)

  (def instrumento-1
    {m2-1 {:out 0 :amp 2}
     m2-2 {:out 1 :amp 2}
     m2-3 {:out 0 :amp 2}
     m2-4 {:out 1 :amp 2}})
  #_(s/valid? ::static-samples-map instrumento-1)
  #_(play-static-samples instrumento-1)
  #_(stop)
  #_(free-all-loaded-samples)
  #_(->> @loaded-samples* (map (comp buffer-size second)) count))
