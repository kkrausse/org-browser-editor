(ns org-browser-editor.roam.sqlite
  (:require
   [promesa.core :as p]
   [org-browser-editor.util :refer [->uuid]])
  )

(defonce sql-worker (new js/Worker "./worker.sql-wasm.js"))

(def sql-promises*
  "atom of map of id -> deffered promises which will
  be resolve!'d when id response happens"
  (let [promise-map (atom {})]
    (doto sql-worker
      (aset "onerror"
            (fn [e] (js/console.error "worker error" e)))
      (aset "onmessage"
            (fn handle-worker-message [m]
              (let [{:as data :keys [id]} (js->clj (.-data m) :keywordize-keys true)]
                (swap! promise-map
                       (fn [pm]
                         (if (contains? pm id)
                           (do (p/resolve! (get pm id) (assoc data
                                                              :m m))
                               (swap! promise-map dissoc id))
                           (js/console.error "worker no promise for id!" data))))))))
    promise-map))

(defn sql-post-msg! [msg]
  (let [id (->uuid)
        p (p/deferred)]
    (swap! sql-promises* assoc id p)
    (.postMessage sql-worker (clj->js (assoc msg :id id)))
    p))

(defn db-exec [sql & [param-map]]
  (p/let [r (sql-post-msg!
             {:action "exec"
              :sql sql
              :params (some->> param-map
                               (into {}
                                 (map (fn [[k v]]
                                        [(str k) v]))))})]
    (if (:error r)
      (js/console.error "sql error! " (clj->js {:error (:error r)
                                                :r r
                                                :sql sql
                                                :params param-map}))
      (let [{:keys [columns values]} (-> r :results first)
            cols                     (map keyword columns)
            parsed                   (->> values
                                          (mapv (partial zipmap cols)))]
        ;; (klog "db ex" sql parsed)
        parsed))))
