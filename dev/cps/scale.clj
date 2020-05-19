(ns cps.scale
  (:require
   [taller-abierto.internal :refer [validate]]
   [clojure.spec.alpha :as s]))

(s/def ::bounded-ratio ratio?)
(s/def ::bounding-period number?)
(s/def ::degree
  (s/keys :req-un [::bounded-ratio ::bounding-period]))
(s/def ::scale (s/and not-empty (s/* ::degree)))


(defn- get-transp-fn [scale-period]
  (if (>= scale-period 0) * /))

(defn- transpose-by [bounding-period scale-period]
  (apply (get-transp-fn scale-period)
         1 ;; important when transp-fn is division
         (repeat (Math/abs scale-period) bounding-period)))

(defn deg->freq
  "Given a `scale` as spec'd above, a `base-freq` and a `degree`,
  get a frequency corresponding to the degree.
  The `base-freq` is the frequency that which is closest to degrees 0 and -1.
  Degrees are zero-based.
  This function supports non-octave repeating scales. We call the interval of
  repetition the `period` of the scale. That is why for the `scale` each of its
  degrees must have `:bounding-period` and `:bounded-ratio` keys.
  If the degree is outside the range of the scale i.e. 7 in a 7 note scale, then
  the frequency will be transposed to the corresponding `period`.
  For convenience the `:period` key can be used to transpose the resulting
  frequency. I.e 1 will play the degree one `period` above."
  [scale base-freq degree
   & {:keys [period] :or {period 0}}]
  {:pre [(validate ::scale scale)]}
  (let [scale-len (count scale)
        period* (+ period (quot degree scale-len))
        degree* (mod degree scale-len)
        {:keys [bounded-ratio bounding-period]} (nth scale degree*)
        period-transp (transpose-by bounding-period period*)]
    (* period-transp bounded-ratio base-freq)))

(do
  (defn intervals->degs [base-deg intervals]
    (reduce #(conj %1 (+ (last %1) %2))
            [base-deg]
            intervals)))
(comment
  (require '[conversions :refer [ratio->cents cps->midi midi->cps]]
           '[clojure.test :refer [testing is]]
           '[hexany :as cps])

  (do (def hexany
        (->> [6 7 5 9]
             (cps/->set 2)
             cps/set->maps
             (cps/bound-ratio 2)
             (cps/maps->data :bounded-ratio)))

      (s/explain ::scale (hexany :scale)))

  (testing deg->freq
    (testing "octaves up"
      (is (= '(35/32 35/16 35/8)
             (->> [0 6 12] (map #(deg->freq (hexany :scale) 1 %)))))
      (is (= '(350/32 350/16 350/8)
             (->> [0 6 12] (map #(deg->freq (hexany :scale) 10 %)))))
      (is (= '(35/32 21/16 21/8)
             (->> [0 1 7] (map #(deg->freq (hexany :scale) 1 %))))))
    (testing "octaves down"
      (is (= '(35/128 35/32 35/16)
             (->> [-12 0 6] (map #(deg->freq (hexany :scale) 1 %)))))
      (is (= '(35/512 35/128 35/32)
             (->> [-24 -12 0] (map #(deg->freq (hexany :scale) 1 %))))))
    (testing "period"
      (is (= '(35/16 35/8 35/4)
             (->> [0 6 12] (map #(deg->freq (hexany :scale) 1 % :period 1)))))))

  (testing intervals->degs
    (testing "upward sequence"
      (is (= '[0 1 3 6 10 15]
             (intervals->degs 0 [1 2 3 4 5]))))
    (testing "downward sequence"
      (is (= '[0 -1 -3 -6 -10 -15]
             (intervals->degs 0 [-1 -2 -3 -4 -5]))))
    (testing "mixed sequence"
      (is (= '[0 -1 1 -2 2 -3]
             (intervals->degs 0 [-1 2 -3 4 -5]))))
    (testing "starting on a different base-deg"
      (is (= '[1 0 2 -1 3 -2]
             (intervals->degs 1 [-1 2 -3 4 -5]))))))
