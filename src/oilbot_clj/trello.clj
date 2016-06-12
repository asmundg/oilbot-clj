(ns oilbot-clj.trello
  (:require
   [aleph.http :as http]
   [cheshire.core :as json]
   [byte-streams :as bs]
   [oilbot-clj.cache :as cache]
   [oilbot-clj.elo :as elo])
  (:gen-class))

(def apiroot "https://api.trello.com/1")
(def key "FOO")
(def token "FOO")
(def authorize-url (str apiroot "/authorize?expiration=never&name=Oilbot&scope=read,write&response_type=token&key=" key))

;; Add auth values to the given query parameters
(defn add-auth [query-params]
  (assoc query-params :token token :key key))

;; HTTP GET a path from the API with a query param map
(defn fetch [path query-params]
  (let [url (str apiroot path)]
    (-> @(http/get url {:query-params (add-auth query-params)})
        :body
        bs/to-string
        (json/parse-string true))))

;; Return seq of cards matching the query
(defn find-cards []
  (filter (comp not nil? :checklist)
          (->> (fetch "/search" {:query "handleliste"})
               :cards
               (map #(assoc {}
                            :id (:id %)
                            :checklist (first (:idChecklists %)))))))

;; Given a card, return the list map
(defn get-list [c]
  (list-cacheattr (fetch (str "/checklists/" (:checklist c)) {})))

;; Given a card, list, item and position, update item with that
;; position
(defn set-sort! [cardid listid itemid pos]
  @(http/put (str apiroot "/cards/" cardid "/checklist/" listid "/checkitem/" itemid "/pos")
             {:query-params (add-auth {:value pos})}))

;; Return map containing cache data for a card. Use count to figure
;; out whether a list has been changed or reordered (until we can be
;; more clever).
(defn list-cacheattr [l]
  {:id (:id l)
   :count (count (:checkItems l))
   :items (map item-cacheattr (:checkItems l))})

;; Return a map containing cache data for a list item
(defn item-cacheattr [i]
  (select-keys i [:name :pos :id]))

(defn train! [l]
  (doseq [p (partition 2 1 (sort-by :pos l))]
    (let [loser (:id (first p))
          winner (:id (second p))
          result (elo/update-r {:winner (cache/get-sortkey winner)
                                :loser (cache/get-sortkey loser)})]
      (cache/set-sortkey! winner (:winner result))
      (cache/set-sortkey! loser (:loser result))))
  (cache/set! (:id l) l))

(defn sort! [c l]
  (doseq [i (:items l)]
    (set-sort! (:id c)
               (:checklist c)
               (:id i)
               (cache/get-sortkey (:id i))))
  (cache/set! (:id l) (get-list c)))

;; main dispatch
(defn handle-card! [c]
  (let [l (get-list c)
        cached (cache/get (:id l))]
    (cond
      ;; No change
      (= cached l) nil
      ;; Unknown (unsorted) list
      (= cached nil) (sort! c l)
      ;; Reordered list with same number of items
      (= (:count cached) (:count l)) (train! l)
      ;; List with new (unsorted) items
      :else (sort! c l))))

(defn handle-all-cards! []
  (doseq [c (find-cards)]
    (handle-card! c)))
