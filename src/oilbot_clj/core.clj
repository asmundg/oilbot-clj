(ns oilbot-clj.core
  (:require
   [aleph.http :as http]
   [cheshire.core :as json]
   [byte-streams :as bs]
   [manifold.stream :as s]
   [taoensso.carmine :as car]
   [oilbot-clj.elo :as elo])
  (:gen-class))

(def id-counter (atom 0))
(def slack-token (System/getenv "SLACK_TOKEN"))
(def redis-host (or (System/getenv "REDIS_HOST") "localhost"))

(defmacro wcar* [& body] `(car/wcar {:pool {} :spec {:host redis-host :port 6379}} ~@body))

(defn get-id []
  (swap! id-counter inc)
  @id-counter)

(defn get-url [api-token]
    (-> @(http/post "https://slack.com/api/rtm.start" {:query-params {:token api-token}})
      :body
      bs/to-string
      (json/parse-string true)
      :url))

(defn connect [url]
  (let [conn @(http/websocket-client url)]
    (do
      (assert @(s/take! conn) {:type "hello"})
      conn)))

(defn make-reply [channel text]
  (json/generate-string {:id (get-id)
                         :type "message"
                         :channel channel
                         :text text}))

(defn get-sortkey [name]
  (Float/parseFloat (or (wcar* (car/get (str "sort-" name))) "400")))

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

(defn handle-message [text]
  (condp re-find text
    #"^a(?:dd)? (.*)" :>> #(str (handle-add! (get % 1)))
    #"^o(?:rder)? (.*) (.*)" :>> #(str (handle-ordering! (get % 1) (get % 2)))
    #"^l(?:ist)?" (str (handle-get))
    #"^clear" (str (handle-reset!))
    #"^ping" "pong"
    #"^redis" (wcar* (car/ping))
    "I have no idea what you're talking about"))

(defn keepalive [conn]
  (let [run (atom true)]
    (do
      (future
        (while @run
          (do
            (Thread/sleep 2000)
            (s/put! conn (json/generate-string {:id (get-id) :type "ping"})))))
      run)))

(defn iterate-event-loop [conn]
  (let [msg (json/parse-string @(s/take! conn) true)]
    (prn msg)
    (case (:type msg)
      "message" (s/put! conn (make-reply (:channel msg)
                                         (handle-message (:text msg))))
      nil (if (nil? msg) (throw (Exception. "Lost connection")))
      (prn (:type msg)))))

(defn -main []
  (while true
    (prn "Starting")
    (let [conn (-> (get-url slack-token)
                   connect)
          keepalive-run (keepalive conn)]
      (try
        (while true
          (iterate-event-loop conn))
        (catch Exception e
          (prn e)
          (reset! keepalive-run false)
          (Thread/sleep 1000))))))
