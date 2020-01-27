(ns taller-abierto.graphs.specs
  (:require [clojure.spec.alpha :as s]
            [overtone.core :refer [buffer?]]))

(defn atom? [x] (instance? clojure.lang.IAtom x))

;;; general
(s/def ::amp number?)
(s/def ::out number?)
(s/def ::ch number?)
(s/def ::sample buffer?)

;;; static-samples-map
(s/def ::static-samples-map
  (s/map-of ::sample (s/keys :opt-un [::out ::amp])))


;;; graph
(s/def ::instrument buffer?)
(s/def ::instruments (s/* ::instrument))
(s/def ::synth (s/and var?
                      (comp fn? var-get)
                      #(-> %
                             meta :arglists
                             first            ;; &
                             second :keys     ;; optional arguments
                             (->> (map keyword))
                             set (contains? :data))))

(s/def ::params (s/and (comp atom? var-get) (comp map? deref var-get)))
(s/def ::node* (s/keys :req-un [::instruments ::synth]
                       :opt-un [::params]))
(s/def ::node (s/and var? #(s/valid? ::node* (var-get %))))
(s/def ::edges (s/coll-of ::node))
(s/def ::graph (s/map-of ::node ::edges ))


;;; state
(s/def ::history (s/coll-of ::node))
(s/def ::xos (s/and var?
                    #(s/valid? (s/coll-of boolean) (var-get %))))
(s/def ::voicef (s/or ::fn fn?
                      ::set (s/and set? (s/coll-of int?))))
(s/def ::state* (s/keys :req-un [::history] :opt-un [::xos ::voicef]))
(s/def ::state (s/and atom? #(->> % deref (s/valid? ::state*))))
