(ns oilbot-clj.core
  (:require
   [aleph.http :as http]
   [cheshire.core :as json]
   [byte-streams :as bs]
   [manifold.stream :as s]
   [taoensso.carmine :as car])
  (:gen-class))

(def id-counter (atom 0))

(defmacro wcar* [& body] `(car/wcar {:pool {} :spec {:host "redis" :port 6379}} ~@body))

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

(defn handle-add! [item]
  (let [list (wcar* (car/get "list"))
        new-list (if (nil? list)
                   [item]
                   (conj list item))]
    (wcar* (car/set "list" new-list))
    new-list))

(defn handle-reset [item]
  (wcar* (car/set "list" []))
  "OK")

(defn handle-message [text]
  (condp re-find text
    #"add (.*)" :>> #(str (handle-add! (get % 1)))
    #"list" (str (wcar* (car/get "list")))
    #"clear" (str (wcar* (car/set "list" [])))
    #"ping" "pong"
    #"redis" (wcar* (car/ping))
    "I have no idea what you're talking about"))

(defn -main []
  (let [conn (-> (get-url (System/getenv "SLACK_TOKEN"))
                 connect)]
    (future (while true (do (Thread/sleep 1000) (s/put! conn (json/generate-string {:id (get-id) :type "ping"})))))
    (while true
      (let [msg (json/parse-string @(s/take! conn) true)]
        (case (:type msg)
          "message" (s/put! conn (make-reply (:channel msg) (handle-message (:text msg))))
          (prn (:type msg)))))))
