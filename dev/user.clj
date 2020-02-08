(ns user
  (:require clojure.pprint
            [clojure.string :as string]
            [clojure.tools.namespace.repl :refer [refresh set-refresh-dirs]]
            [overtone.core :as o :refer :all]))

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
(defn connect []
  (cond
    (or (o/server-connected?) (o/server-connecting?)) :already-connected
    windows? (o/connect-external-server)
    :else (o/boot-external-server)))

(defn test-sound []
  (demo (sin-osc 400)))
