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
