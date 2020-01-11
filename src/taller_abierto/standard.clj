(ns taller-abierto.standard)

(def ^:dynamic *out-channels* 2)

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

(defn get-instruments [state]
  (-> @state :history last var-get :instruments))

(defn get-synth [state]
  (-> @state :history last var-get :synth))

(defn xo-play? [state index]
  (let [xos (var-get (@state :xos))
        len (count xos)]
    (nth xos (mod index len))))

(defn mirror [xs] (concat xs (reverse xs)))

(defn rrange
  "Random range"
  [bottom top]
  (-> (- top bottom)
      rand-int
      (+ bottom)))

(defn +- [base n] (+ base (* (rand-nth [1 -1]) n)))
