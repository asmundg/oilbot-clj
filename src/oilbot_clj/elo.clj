(ns oilbot-clj.elo
  (:require
   [clojure.math.numeric-tower :as math])
  (:gen-class))

(def K 32)

(defn e [ra rb]
  (/ 1 (+ 1 (math/expt 10 (/ (- rb ra) 400)))))

(defn r*a [ra sa ea]
  (+ ra (* K (- sa ea))))

(defn update-r [{:keys [winner loser]}]
  {:winner (r*a winner 1 (e winner loser))
   :loser (r*a loser 0 (e loser winner))})

