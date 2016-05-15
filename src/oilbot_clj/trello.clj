(ns oilbot-clj.trello
  (:require
   [aleph.http :as http]
   [cheshire.core :as json]
   [byte-streams :as bs])
  (:gen-class))

(def apiroot "https://api.trello.com/1")
(def key "FOO")
(def token "FOO")
(def authorize-url (str apiroot "/authorize?expiration=never&name=Oilbot&scope=read,write&response_type=token&key=" key))

(defn add-auth [query-params]
  (assoc query-params :token token :key key))

(defn fetch [path query-params]
  (let [url (str apiroot path)]
    (-> @(http/get url {:query-params (add-auth query-params)})
        :body
        bs/to-string
        (json/parse-string true))))

(defn find-lists []
  (->> (fetch "/search" {:query "handleliste"})
       :cards
       (mapcat :idChecklists)))

(defn get-list [listid]
  (fetch (str "/checklists/" listid) {}))

(defn set-sort [cardid listid itemid pos]
  @(http/put (str apiroot "/cards/" cardid "/checklist/" listid "/checkitem/" itemid "/pos")
             {:query-params (add-auth {:value pos})}))

(let [l (-> (find-lists)
            first
            get-list)
      item (->> l
                :checkItems
                (sort-by :pos)
                first)]
  (set-sort (:idCard l)
            (:idChecklist item)
            (:id item)
            99999999999))
