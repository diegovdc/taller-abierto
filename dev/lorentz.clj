(ns lorentz
  (:require [clojure.spec.alpha :as s]
            [quil.core :as q]))

(def dt 0.01)
(defn dx [a x y] (* dt a (- y x)))
(defn dy [b x y z] (* dt (- (* x (- b z)) y)))
(defn dz [c x y z] (* dt (- (* x y) (* c z))))


(do
  (defn init-system
    "Creates a function with a set of initial values, that will compute their
  value of the lorentz attractor at a certain point in time.
  It will memoize previous states and will only calculate new states as necessary.

  It also returns the min and max values for each coordinate in the system at
  the latest point that has been computed.

  Usage:
  `(def lor (lorentz-system))`
  `(lor 5000)`"
    [& {:keys [x y z a b c dt]
        :or {x 0.01
             y 0.03
             z 0.02
             a 10.4
             b 29
             c 8/3
             dt 0.01}}]
    (let [state (atom {:x x :y y :z z :points []})
          get-max (fn [maxs coord v] (max (get maxs coord 0) v))
          get-min (fn [mins coord v] (min (get mins coord 0) v))]
      (fn [i]
        (if-let [points (nth (@state :points) i nil)]
          (let [{:keys [mins maxs]} @state]
            {:point points
             :mins mins
             :maxs maxs})
          (let [{:keys [points] :as s} @state
                init (count points)
                {:keys [mins maxs points]
                 :as next-state} (->> (range init (inc i))
                 (reduce
                  (fn [{:keys [x y z points mins maxs]} _]
                    (let [x (+ x  (dx a x y))
                          y (+ y (dy b x y z))
                          z (+ z (dz c x y z))]
                      {:x x :y y :z z
                       :maxs {:x (get-max maxs :x x)
                              :y (get-max maxs :y y)
                              :z (get-max maxs :z z)}
                       :mins {:x (get-min mins :x x)
                              :y (get-min mins :y y)
                              :z (get-min mins :z z)}
                       :points (conj points [x y z])}))
                  s))]
            (reset! state next-state)
            {:point (last points)
             :mins mins
             :maxs maxs}))))))


(defn- bounds* [lorentz-val coord]
  [(get-in lorentz-val [:mins coord] 0)
   (get-in lorentz-val [:maxs coord] 0)])



(defn bound
  [lorentz-val coord min max]
  (let [[low1 high1] (bounds* lorentz-val coord)
        v ((lorentz-val :point) (case coord :x 0 :y 1 :z 2 0))]
    (q/map-range v low1 high1 min max)))


(defn coord [lorentz-val coord]
  (-> lorentz-val :point (nth (case coord :x 0 :y 1 :z 2 0) 0)))
(defn x [lorentz-val] (-> lorentz-val (coord :x)))
(defn y [lorentz-val] (-> lorentz-val (coord :y)))
(defn z [lorentz-val] (-> lorentz-val (coord :z)))
(comment
  (def lor (lorentz-system))
  (coord (lor 500) :y)

  ;; quil visualization

  (do
    (defn setup []
      (q/color-mode :hsb))
    (def state (atom {:x 0.01 :y 0.03 :z 0.02 :points []}))

    (def a 10.4)
    (def b 29)
    (def c 8/3)
    (def dt 0.007)
    (defn draw []
      (q/background 0)
      (q/stroke-weight 3)
      (q/no-fill)
      (let [{:keys [x y z points]} @state
            size 8]
        (q/with-translation [(/ (q/width) 2)
                             (/ (q/width) 2)]
          (q/begin-shape)
          (doseq [[i p*] (map-indexed (fn [i x] [i x]) points)]
            (q/stroke (mod (+ (count points) (* 0.01 i (rand 3))) 255) 255 255)

            (apply q/vertex (->> p* (map #(* size %)))))
          (q/end-shape)
          (reset! state {:x (+ x  (dx a x y))
                         :y (+ y (dy b x y z))
                         :z (+ z (dz c x y z))
                         :points (conj points [x y z])})))))

  (q/defsketch lorentz
    :size [500 500]
    :renderer :p3d
    :setup setup
    :draw draw))
