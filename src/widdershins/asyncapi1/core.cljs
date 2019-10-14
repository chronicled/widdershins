(ns widdershins.asyncapi1.core
  (:require 
    [shodan.console :as log]))

(defn- convert-message
  [message-mode message]
  (when (some? message)
    {message-mode message}))

(defn- convert-topic
  [topic-name {:keys [parameters publish subscribe] :as topic}]
  (let [pubsub (cond-> []
                 (some? publish) (conj (assoc publish ::mode :publish))
                 (some? subscribe) (conj (assoc subscribe ::mode :subscribe)))
        queues (merge (group-by :x-exchange pubsub)
                      (group-by :x-queue pubsub)
                      {nil (filter #(and (nil? (:x-exchange %))
                                         (nil? (:x-queue %)))
                                   pubsub)})]
    (->> queues
         (map (fn [[queue exchange-messages]]
                {:topic (str (when (some? queue) (str (name queue) "."))
                             (name topic-name))
                 :messages (->> exchange-messages
                                (map (fn [{mode ::mode :as message}]
                                       {mode (dissoc message ::mode)}))
                           (into {}))
                 :parameters parameters}))
         (remove #(empty? (:messages %))))))

(defn- convert-topics
  [{:keys [x-exchanges x-queues topics]}]
  (->> topics
       (mapcat (fn [[topic-name topic]]
                 (convert-topic topic-name topic)))
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
