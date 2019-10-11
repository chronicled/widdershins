(ns widdershins.asyncapi1.core)

(defn- convert-message
  [message-mode message]
  (when (some? message)
    {message-mode message}))

(defn- convert-topic
  [{:keys [parameters publish subscribe] :as topic}]
  {:messages (merge (convert-message :publish publish)
                    (convert-message :subscribe subscribe)) 
   :parameters parameters})

(defn- convert-topics
  [{:keys [x-exchanges x-queues topics]}]
  (->> topics
       (map (fn [[topic-name topic]]
              (when topic [topic-name (convert-topic topic)])))
       (remove nil?)
       (into {})))

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
