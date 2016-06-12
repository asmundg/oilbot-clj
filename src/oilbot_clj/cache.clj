(ns oilbot-clj.cache
  (:require
   [taoensso.carmine :as car])
  (:gen-class))

(def redis-host (or (System/getenv "REDIS_HOST") "localhost"))

(defmacro wcar* [& body] `(car/wcar {:pool {} :spec {:host redis-host :port 6379}} ~@body))

(defn get [key]
  (wcar* (car/get key)))

(defn set! [key val]
  (wcar* (car/set key val)))

(defn get-sortkey [name]
  (let [key (Float/parseFloat (or (wcar* (car/get (str "sort-" name))) "400"))]
    (if (= key 400.0)
      (+ key (rand 10))
      key)))

(defn set-sortkey! [name val]
  (wcar* (car/set (str "sort-" name) val)))

(defn handle-get []
  (reverse (sort-by get-sortkey (wcar* (car/get "list")))))

(defn handle-add! [item]
  (let [list (wcar* (car/get "list"))
        new-list (if (nil? list)
                   [item]
                   (conj list item))]
    (do
      (wcar* (car/set "list" new-list))
      (handle-get))))

(defn handle-ordering! [first second]
  (let [result (elo/update-r {:winner (get-sortkey first)
                              :loser (get-sortkey second)})]
    (set-sortkey! first (:winner result))
    (set-sortkey! second (:loser result))
    (handle-get)))

(defn handle-reset! []
  (wcar* (car/set "list" []))
  "OK")
