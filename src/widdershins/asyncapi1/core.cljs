(ns widdershins.asyncapi1.core
  (:require 
    [shodan.console :as log]))

(defn- convert-topic
  [{section-info :info}
   {topic-name :key
    parameters :parameters 
    publish :publish
    subscribe :subscribe}]
  (let [section-hash (hash section-info)
        pubsub (cond-> []
                 (some? publish) (conj (assoc publish ::mode :publish))
                 (some? subscribe) (conj (assoc subscribe ::mode :subscribe)))
        queues (merge (group-by :x-exchange pubsub)
                      (group-by :x-queue pubsub)
                      {nil (filter #(and (nil? (:x-exchange %))
                                         (nil? (:x-queue %)))
                                   pubsub)})]
    (->> queues
         (map (fn [[queue exchange-messages]]
                (as-> {} topic-ent
                  (assoc topic-ent
                         :topic (str (when (some? queue) (str (name queue) "."))
                             (name topic-name))
                         :messages (->> exchange-messages
                                (map (fn [{mode ::mode :as message}]
                                       {mode (dissoc message ::mode)}))
                                (into {}))
                         :parameters parameters)
                  (assoc topic-ent :slug (str section-hash "-" (:topic topic-ent))))))
         (remove #(empty? (:messages %))))))

(defn- convert-topics
  [{:keys [x-exchanges x-queues topics] :as section}]
  (->> topics
       (mapcat (fn [[topic-name topic]]
                 (convert-topic section (assoc topic :key topic-name))))
       (sort-by :topic)))

(defn- convert-section
  [section]
  (let [section-title (get-in section [:info :title] "Untitled")
        section-topics (convert-topics section)
        section-body {:description (get-in section [:info :description])
                      :externalDocs (get-in section [:info :externalDocs])
                      :topics section-topics}]
    [section-title section-body]))

(defn- do-convert-to-toc
  [sources]
  (->> sources
       (map convert-section)
       (into {})))

(defn ^:export convert-to-toc
  [sources]
  (-> sources
      (js->clj :keywordize-keys true)
      (do-convert-to-toc)
      (clj->js)))
