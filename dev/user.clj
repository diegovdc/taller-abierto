(ns user
  (:require
   [clojure.pprint]
   [clojure.tools.namespace.repl :refer [set-refresh-dirs refresh refresh-all]]))

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

(defn connect []
  (eval '(do (require '[overtone.core :refer :all])
             (boot-external-server))))

(defn test-sound []
  (eval '(do (require '[overtone.core :refer :all])
             (demo (sin-osc 400)))))
