(ns visuals.durations
  (:require [quil.core :as q]
            [taller-abierto.internal :refer [validate]]
            [quil.middleware :as m]
            [overtone.at-at :refer [now]]
            [clojure.spec.alpha :as s]))

(s/def ::start-time int?)
(s/def ::total-dur number?)
(s/def ::name (s/or :s (s/nilable string?)
                    :k (s/nilable keyword?)))
(s/def ::color* (s/and int? #(>= % 0) #(<= % 255)))
(s/def ::color (s/and (s/* ::color*) #(= 3 (count %))))
(s/def ::height int?)
(s/def ::position int?)
(s/def ::end-time int?)
(s/def ::elapsed number?)
(s/def :dur-bar/name string?)
(s/def ::input-event (s/keys :req-un
                             [::start-time ::total-dur ::name]))
(s/def ::input-events (s/* ::input-event))
(s/def :dur-bar/event (s/keys :req-un [::total-dur
                                       ::start-time
                                       :dur-bar/name
                                       ::color
                                       ::height
                                       ::position
                                       ::end-time
                                       ::elapsed]))


(def height 2100)
(def width 1000)

(def gen-color
  (memoize (fn [name] [(rand-int 255) (rand-int 255) (rand-int 255)])))

(defn ensure-event-styles [height events]
  (->> events
       (map-indexed
        (fn [i ev]
          (-> ev
              (clojure.core/update :name #(or % (str "canon " i)))
              (#(assoc % :color (gen-color (% :name))))
              (assoc :height height)
              (assoc :position (* i height)))))))

(defn add-end-time
  "Times must be in milliseconds"
  [{:keys [start-time total-dur] :as event}]
  (assoc event :end-time (+ start-time total-dur)))

(defn scale-event-elapsed
  "Times must be in milliseconds"
  [width now {:keys [end-time total-dur] :as event}]
  (let [elapsed (- total-dur (- end-time now))]
    (assoc event :elapsed (* elapsed (/ width total-dur)))))

()

(do
  (defn update* [state]
    (-> state
        (update :events #(ensure-event-styles 32 %))
        (update :events #(map add-end-time %))
        (update :events #(map (partial scale-event-elapsed width (now)) %)))))

(defn setup [events]
  (fn []
    #_(q/frame-rate 15) ;; preserving resources
    (update* {:events events})))

(defn dur-bar
  [{:keys [elapsed name color position height] :as event}]
  {:pre [(validate :dur-bar/event event)]}
  (apply q/fill (conj color 100))
  (q/rect 0 position width height)
  (apply q/fill (conj color 255))
  (q/rect 0 position (- width (- width elapsed)) height)
  (q/fill 255 255 255)
  (q/text-size 30)
  (q/text name 0 (- (+ height position) 5)))

(defn draw [state]
  (q/background 0)
  (q/no-stroke)
  (doseq [event (state :events)]
    (dur-bar event)))


(defn event-durations
  [events]
  {:pre (validate ::input-events events)}
  (q/defsketch event-durations*
    :bgcolor "#000"
    :title "Event Durations"
    :size [width height]
    :setup (setup events)
    :draw draw
    :update update*
    :middleware [m/fun-mode]))


(comment
  (def test-data [{:total-dur 10000 :start-time (now)}
                  {:total-dur 20000 :start-time (now)}])
  (event-durations test-data))
