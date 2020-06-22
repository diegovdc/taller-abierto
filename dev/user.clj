(ns user
  (:require [clojure.pprint]
            [clojure.string :as string]
            [clojure.tools.namespace.repl :refer [refresh set-refresh-dirs]]
            [taoensso.timbre :as log]
            [overtone.core :as o :refer :all]
            [clojure.java.io :as io])
  (:import [java.time LocalDateTime]
           [java.util Date]
           [java.text SimpleDateFormat]))

(set-refresh-dirs "src" "test")

(defn reset [] (refresh))

(defn make-spy [printer]
  (fn [& args]
    (let [off (some #(= % :mute) args)
          quiet (some #(= % :quiet) args)
          val (last args)]
      (if (or off)
        val ;; returns val when muted and/or not on debug

        (let [msgs (butlast args)]
          ((if quiet identity printer)
           (concat
            (->> msgs (mapv (fn [msg]
                              (if (fn? msg)
                                (msg val)
                                msg))))
            [val]))))
      val)))

(defn spy
  "Example: (spy :some-symbol some-func return-value) => return-value ;; logs everything to the console"
  [& args]
  (apply
   (make-spy #(doseq [x %] (clojure.pprint/pprint x)))
   args))


(defn spy->
  "Spy for a thread first macro"
  [& args]
  (apply spy (reverse args)))


(defn ignore [& args] (last args))

(defn ignore-> [& args] (first args))

(def data (atom {}))

(defn capture
  ([key val] ((capture key) val))
  ([key]
   (fn [val]
     (swap! data #(assoc % key val)))))

(def windows? (string/includes? (System/getProperty "os.name")
                                "Windows"))

(def outs (atom 2))

(defn connect [outs*]
  (reset! outs outs*)
  (cond
    (or (o/server-connected?) (o/server-connecting?)) :already-connected
    windows? (o/connect-external-server)
    :else (o/boot-external-server)))

(defn test-sound []
  (demo (sin-osc 400)))

(def ^:dynamic *drives* {:linux "/media/diego/Music/"
                         :windows "F:\\"})

(defn make-path [path]
  (let [drive (if windows? (*drives* :windows) (*drives* :linux))]
    (if windows? (string/replace (str drive path) #"/" "\\\\")
        (str drive path))))

(defn iso-now []
  (.format (SimpleDateFormat. "yyyy-MM-dd'T'HH_mm_ss")
           (Date.)))

(defn rec [dir]
  (let [path (make-path (str "music/taller-abierto/sc/" dir "/" (iso-now) ".wav"))]
    (io/make-parents path)
    (println "Recording on:" path)
    (o/recording-start path)))

(defn stop-rec [] (println (o/recording-stop)))


;; Logger config
(log/merge-config!
 {:output-fn
  (fn [data]
    (let [{:keys [level ?err vargs msg_
                  ?ns-str ?file hostname_
                  timestamp_ ?line]} data]

      (str "[" (or ?ns-str ?file "?") ":" (or ?line "?") "] "
           (clojure.string/upper-case (name level))" - "
           (clojure.string/join " " vargs)
           (when-let [err ?err]
             (str "\n" (log/stacktrace err {}))))))})
