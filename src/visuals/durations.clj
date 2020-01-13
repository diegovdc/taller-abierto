(ns visuals.durations
  (:require [quil.core :as q]
            [taller-abierto.internal :refer [validate]]
            [quil.middleware :as m]
            [overtone.at-at :refer [now]]
            [clojure.spec.alpha :as s]))

(s/def ::start-time int?)
(s/def ::total-dur number?)
(s/def ::name (s/or :s string? :k keyword?))
(s/def ::color* (s/and int? #(>= % 0) #(<= % 255)))
(s/def ::color (s/and (s/* ::color*) #(= 3 (count %))))
(s/def ::height int?)
(s/def ::position int?)
(s/def ::end-time int?)
(s/def ::elapsed number?)
(s/def :dur-bar/name string?)
(s/def ::input-event (s/keys :req-un [::start-time ::total-dur ::name]))
(s/def ::input-events (s/* ::input-event))
(s/def :dur-bar/event (s/keys :req-un [::total-dur
                                       ::start-time
                                       :dur-bar/name
                                       ::color
                                       ::height
                                       ::position
                                       ::end-time
                                       ::elapsed]))

;;; API
(declare add-event! ;; takes an ::input-event
         start-event-durations-gui ;; takes an ::input-events or a 0-arity call
         )

;;; window styles
(def ^:private width 1000)
(def ^:private height 2100)


;;; Implementation
(def ^:private new-events-queue (atom []))

(defn add-event!
  [event]
  {:pre [(validate ::input-event event)]}
  (swap! new-events-queue conj event))

(defn- conj-new-events! [state]
  (let [events @new-events-queue]
    (reset! new-events-queue [])
    (update state :events concat events)))

(defonce ^:private gen-color
  (memoize (fn [name] [(rand-int 255) (rand-int 255) (rand-int 255)])))

(defn- has-finished?
  [{:keys [unscaled-elapsed total-dur]}]
  (> unscaled-elapsed total-dur))

(defn- ensure-event-styles [height events]
  (->> events
       (map-indexed
        (fn [i ev]
          (-> ev
              (assoc
               :has-finished? (has-finished? ev)
               :printable-name (str (name (ev :name))
                                    (when (:has-finished? ev)
                                      " (finished)"))
               :color (gen-color (ev :name))
               :height height
               :position (* i height)
               :unelapsed-alpha (if-not (:has-finished? ev) 100 0)
               :elapsed-alpha (if-not (:has-finished? ev) 255
                                      (dec  (ev :elapsed-alpha)))))))))

(defn- remove-finished-event [events]
  (remove #(>= 0 (get % :elapsed-alpha 255)) events))

(defn- add-end-time
  "Times must be in milliseconds"
  [{:keys [start-time total-dur] :as event}]
  (assoc event :end-time (+ start-time total-dur)))

(defn- scale-event-elapsed
  "Times must be in milliseconds"
  [width now {:keys [end-time total-dur] :as event}]
  (let [elapsed (- total-dur (- end-time now))]
    (assoc event
           :elapsed (* elapsed (/ width total-dur))
           :unscaled-elapsed elapsed)))

(defn- update* [state]
  (-> state
      conj-new-events!
      (update :events #(map add-end-time %))
      (update :events #(map (partial scale-event-elapsed width (now)) %))
      (update :events remove-finished-event)
      (update :events #(ensure-event-styles 32 %))))
#_(update* {:events test-data})

(defn- setup [events]
  (fn []
    #_(q/frame-rate 15) ;; preserving resources
    (update* {:events events})))

(defn- dur-bar
  [{:keys [elapsed
           printable-name
           color
           position
           height
           unelapsed-alpha
           elapsed-alpha] :as event}]
  {:pre [(validate :dur-bar/event event)]}
  (apply q/fill (conj color unelapsed-alpha))
  (q/rect 0 position width height)
  (apply q/fill (conj color elapsed-alpha))
  (q/rect 0 position (- width (- width elapsed)) height)
  (q/fill 255 255 255)
  (q/text-size 30)
  (q/text printable-name 0 (- (+ height position) 5)))

(defn- draw [state]
  (q/background 0)
  (q/no-stroke)
  (when (nil? (seq (state :events)))
    (q/text-size 30)
    (q/text "no events are playing now" 0 25))
  (doseq [event (state :events)]
    (dur-bar event)))


(defn- start-event-durations-gui
  ([] (start-event-durations-gui []))
  ([events]
   {:pre [(validate ::input-events events)]}
   (q/defsketch event-durations
     :bgcolor "#000"
     :title "Event Durations"
     :size [width height]
     :setup (setup events)
     :draw draw
     :update update*
     :middleware [m/fun-mode])))

(comment
  (def test-data [{:total-dur 10000 :start-time (now) :name "canon 1"}
                  {:total-dur 20000 :start-time (now) :name "canon 2"}])
  (start-event-durations-gui test-data)
  (add-event! {:total-dur 10000 :start-time (now) :name (str "canon " (rand-int 20)) }))
