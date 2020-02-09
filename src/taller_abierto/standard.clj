(ns taller-abierto.standard)

(def ^:dynamic *out-channels* 4)

(defn ch
  "The different channels for a pan-az ugen"
  []
  (case *out-channels*
    2 {1 0.25
       2 0.75}
    4 {1 -0.25
       2 0.25
       3 0.75
       4 1.25}))

(defn get-last
  "Get last node"
  [state]
  (-> state deref :history last var-get))

(defn get-instruments [state]
  (-> state get-last :instruments))

(defn get-synth [state]
  (-> state get-last :synth))

(defn get-params [state]
  (some-> state get-last :params var-get deref))

(defn param [state param not-found]
  (let [param* (-> state get-params (get param not-found))]
    (if (fn? param*)
      (param* state)
      param*)))

(defn node-name [state]
  (-> state deref :history last meta :name str))


(defn xo-play? [state index]
  (let [xos (try (var-get (@state :xos))
                 (catch Exception _ nil))
        len (count xos)]
    (when xos
      (nth xos (mod index len)))))

(defn mirror [xs] (concat xs (reverse xs)))

(defn rrange
  "Random range"
  [bottom top]
  (-> (- top bottom)
      rand-int
      (+ bottom)))

(defn +- [base n] (+ base (* (rand-nth [1 -1]) n)))

(defn x-rand [x min max]
  (let [r (range min max)]
    (map (fn [_] (rand-nth r)) (range x))))
