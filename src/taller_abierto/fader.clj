(ns taller-abierto.fader
  (:require [clojure.core.async :as a]
            [overtone.core :as o]))

(defn interpolate [steps from to]
  (let [step-size (-> to (- from) (/ steps))]
    (range from (+ step-size to) step-size)))
(comment (interpolate 10 100 0))

(defn interval-fns [intval fs]
  (let [total (dec (count fs))]
    (a/go-loop [i 0]
      (a/<! (a/timeout intval))
      ((nth fs i))
      (when (< i total)
        (recur (inc i))))))
(comment (interval-fns 100 (repeat 3 #(println "hola"))))

(defn fade [time steps node param from to]
  (let [interval (/ time steps)
        vals* (interpolate steps from to)
        fs (map (fn [v] #(o/ctl node param v)) vals*)]
    (interval-fns interval fs)))

(defn make-fader [time steps node param init-val]
  (let [last-state (atom init-val)]
    (fn fader
      ([to] (fader to time))
      ([to time*]
       (fade time steps node param @last-state to)
       (reset! last-state to)))))
