(ns org-browser-editor.roam.webdav-notes
  (:require
   ["react" :as react]
   ["react-dom/client" :as react-client]
   [meander.epsilon :as me]
   [clojure.pprint]
   [clojure.string :as str]
   [promesa.core :as p]
   [lambdaisland.fetch :as fetch]
   [reagent.core :as rg]
   [org-browser-editor.org-parse :as org-parse]
   [org-browser-editor.slate-rich-org :as rich-org]
   [org-browser-editor.util :as u :refer [klog ptime deep-merge jsonify json-parse]]
   [org-browser-editor.roam.sqlite :refer [db-exec]]
   [org-browser-editor.roam.searchbox :refer [searchbox]]
   [goog.string :refer [format]]
   ;; required to tell the compiler we're using this
   goog.string.format
   ))


(def webdav-url "..")
(def daily-file-prefix "/roam/daily/")
(def dev-mode? (= "localhost"
                  (.. js/window -location -hostname)))
(def disp-max-files (if dev-mode? 2 12))

(defonce state
  (rg/atom {:top-query          [:dailies-q]
            :db-update-i        0
            :db-init?           false
            :browse-hist        []}))

(def login-ref (rg/cursor state [:webdav-login-state]))

(defn login-init! []
  (let [user-pass (js/localStorage.getItem "webdav-login")]
    (swap! login-ref assoc :user-pass user-pass)))

(defn login-set! [user-pass]
  (js/localStorage.setItem "webdav-login" user-pass)
  (swap! login-ref assoc :user-pass user-pass))

(defn login-form []
  [:form {:on-submit (fn [e]
                          (.preventDefault e)
                          (let [user-pass (-> e .-target (aget 0) .-value)]
                            (login-set! user-pass)))}
      [:input {:type        "password"
               :name        "password"
               :placeholder "user:pass"
               :style       {:padding   "0.25em"
                             :font-size 16 ;; so viewport doesn't shrink
                             :width     "auto"
                             :max-width "20ch"}}]
      [:input {:type  "submit"
               :value "Login"}]])

(def webdav-auth
  (rg/track (fn []
              (->> @login-ref
                   :user-pass
                   (js/btoa)
                   (str "Basic " )))))

(comment

  (-> @login-ref)

  (-> @state keys)

  (datetime-str-local)
  (comment
    "2024-11-25T23:14:15.214Z")

  (p/let [fl
          (fileserver-list)]
    (klog "x " (take 10 fl)))

  (login-set! "test:testo")
  )

(defn xml->map [x]
  (if (seq (.-childNodes x))
    (let [r
          (->> x
               (.-childNodes)
               (map (fn [c]
                      [(.-nodeName c)
                       (xml->map c)]))
               (into {}))]
      (if (contains? r "#text")
        (get r "#text")
        r))
    (.-textContent x)))

(defn day-of-week-local []
  (-> (js/Date.)
      (.toLocaleString "en-US", (clj->js { :weekday "long" }))))

(defn datetime-str-local []
  (let [d (js/Date.)
        off (.getTimezoneOffset d)]
    (.toISOString
     (js/Date. (- (.getTime d) (* 60000 off))))))

(defn datetime-parse [x]
  (-> x
      (js/Date.)
      (.toISOString)))

(defn fetch-check [{:keys [status] :as resp}]
  (when-not (and (<= 200 status) (> 300 status))
    (throw (ex-info "bad response"
                    {:response resp})))
  resp)

(defn fileserver-get [href]
  (p/let [r (fetch/get (str webdav-url href)
                       {
                        ;; :headers {"Authorization" @webdav-auth}
                        }
                       )]
    (fetch-check r)
    {:file_contents (:body r)
     :file_modified_time (some-> (get-in r [:headers "last-modified"])
                                 datetime-parse)}))

(defn fileserver-list [& {:keys [url]}]
  (p/let [x (fetch/request
             (or url (str webdav-url "/"))
             {:method :propfind
              ;; :headers {"Authorization" @webdav-auth}
              })]
    (fetch-check x)
    (let [parsed (-> (js/DOMParser.)
                     (.parseFromString (:body x) "application/xml")
                     (.getElementsByTagName "D:response")
                     (->> (map xml->map)
                          (map (fn [x]
                                 {:href (get x "D:href")
                                  :modified (some-> x
                                                    (get-in ["D:propstat" "D:prop" "D:getlastmodified"])
                                                    datetime-parse)
                                  :created (-> x
                                               (get-in ["D:propstat" "D:prop" "D:creationdate"])
                                               (datetime-parse))}))))]
      parsed)))

(defn fileserver-save [href file-contents]
  (p/then (fetch/put (str webdav-url href)
                     {:headers {
                                ;; "Authorization" @webdav-auth
                                "Content-Type" "application/octet-stream"}
                      :body file-contents})
          fetch-check))


(defn cache-file-get-modified []
  (db-exec "
select * from files where file_local_modified
"))


(defn cache-file-upsert! [x]
  (db-exec "
INSERT INTO files (file_name, file_modified_time, file_local_modified, file_contents)
VALUES (:file_name, :file_modified_time, :file_local_modified, :file_contents)
ON CONFLICT(file_name) DO UPDATE SET
    file_modified_time = COALESCE(EXCLUDED.file_modified_time, files.file_modified_time),
    file_local_modified = COALESCE(EXCLUDED.file_local_modified, files.file_local_modified),
    file_contents = COALESCE(EXCLUDED.file_contents, files.file_contents)
"
           x))

(defn cache-node-upsert! [n]
  (db-exec "
INSERT INTO nodes (node_id, node_file_name, node_level, node_start, node_end, node_props, node_title)
VALUES (:node_id, :node_file_name, :node_level, :node_start, :node_end, :node_props, :node_title)
ON CONFLICT(node_id) DO UPDATE SET
    node_file_name = COALESCE(EXCLUDED.node_file_name, nodes.node_file_name),
    node_level = COALESCE(EXCLUDED.node_level, nodes.node_level),
    node_start = COALESCE(EXCLUDED.node_start, nodes.node_start),
    node_end = COALESCE(EXCLUDED.node_end, nodes.node_end),
    node_props = COALESCE(EXCLUDED.node_props, nodes.node_props),
    node_title = COALESCE(EXCLUDED.node_title, nodes.node_title);
" n))

(defn cache-link-upsert! [l]
  (db-exec "
INSERT INTO links (link_src, link_tgt, link_type, link_pos)
VALUES (:link_src, :link_tgt, :link_type, :link_rank)
ON CONFLICT(link_src, link_tgt, link_type) DO UPDATE SET
    link_pos = COALESCE(EXCLUDED.link_pos, links.link_pos);
" l))

(defn file-is-daily? [fname]
  (str/starts-with? fname daily-file-prefix))

(defn parse-hl->title [{:keys [hl-parents headline]} & {:keys [ftitle fname]}]
  (->> hl-parents
       (map :headline)
       (cons headline)
       ((fn [xs]
          (cond-> xs
            true vec
            (not (file-is-daily? fname)) (conj ftitle))))
       (str/join " -> ")))

(comment

  (p/let [r (db-exec "select * from files where file_name like '%-paul_graham.org%'")
          [f & _] r]

    (def testf f)
    (klog "got" (first r)))

  (klog "pg parse"
        (->> testf
             :file_contents
             (org-parse/parse-org :text)
             :headlines
             ))
  (pq "
with ips as (
select *, json_each.value as pp from nodes, json_each(json_extract(node_props, '$.pids'))
)
select * from ips where node_file_name like '%-paul_graham%'")

  (pq "
with recursive ps as (
select *, node_id root_id from
)

select p.value from nodes
join json_each(node_props, '$.cache_pids') p
limit 10
")

  ;; want to be able to set parent and have that show up in search, and be able to nav
  ;; thinking :parents: property works, with space separated links.
  ;; can get emacs script to migrate

  )

(defn cache-node-props-add-parents [props]
  (let [pids (some-> props
                     :subnode_of
                     (str/trim)
                     (str/split #" ")
                     (vec))]
    (cond-> props
      pids (assoc :cache_pids pids))))

(defn cache-file-reindex! [{:as f :keys [file_name file_contents]}]
  (let [{:keys [headlines links] :as pf
         {fid    :id
          ftitle :title
          :as    file-props} :file-props} (org-parse/parse-org :text file_contents)
        records
        (concat
         ;; file node
         [{:node_id        fid
           :node_file_name file_name
           :node_title     ftitle
           :node_start     0
           :node_props     (-> file-props
                               (assoc :search_text ftitle)
                               (cache-node-props-add-parents)
                               jsonify)}]
         (->> headlines
              (keep (fn [{:keys [level id props start end headline] :as x}]
                      (when id
                        ;; (klog "title" (parse-hl->title x :fname file_name :ftitle ftitle))
                        {:node_id        id
                         :node_title     headline
                         ;; make not insert the whole thing?
                         :node_file_name file_name
                         :node_start     start
                         :node_props     (-> props
                                             (assoc :search_text (parse-hl->title x :fname file_name :ftitle ftitle))
                                             (cache-node-props-add-parents)
                                             jsonify)
                         :node_end       end}))))
         (->> links
              (map (fn [{:keys [id hl-src pos alias]}]
                     {:link_src  (or hl-src fid)
                      :link_tgt  id
                      :link_type :ref
                      :link_pos  pos}))))]
    ;; (klog "parsed " pf)
    (p/let [deletef ;; should cascade delete the nodes too
            (db-exec "delete from files where file_name = :file_name" f)
            ;; hack to just reinsert
            _ (cache-file-upsert! f)]
      ;; (klog "recreated " f)
      (->> records
           (keep (fn [d]
                   ;; (klog "inserting" d)
                   (cond
                     (:link_src d) (cache-link-upsert! d)
                     (:node_id d)  (cache-node-upsert! d)
                     :else         (klog "failed upsert for:" d))))
           (p/all)))))


(def db-init? (rg/cursor state [:db-init?]))

(defn ensure-today! []
  (p/let [date-str (subs (datetime-str-local) 0 10)
          file_name (str daily-file-prefix date-str ".org")
          existing (db-exec "select * from files where file_name = :file_name"
                            {:file_name file_name})
          x (when (empty? existing)
              (cache-file-reindex!
               {:file_name file_name
                :file_contents
                (format ":PROPERTIES:
:ID: %s
:END:
#+title: %s
%s
"
                                    (u/->uuid)
                                    date-str
                                    (day-of-week-local))})
              )]
    (when x
      (klog "created today" x existing))
    x))

(defn cache-init! []
  (p/let [_ (db-exec "
PRAGMA foreign_keys = ON;

create table if not exists files (
  file_name text primary key,
  file_modified_time text,
  file_local_modified boolean,
  file_contents text
);

create table if not exists nodes (
  node_id text primary key,
  node_file_name text,
  node_level text,
  node_title text,
  node_start integer, -- pos
  node_end integer, -- null for file nodes
  node_props text,
  foreign key (node_file_name) references files(file_name) on delete cascade
);

create table if not exists links (
  link_src text,
  link_tgt text,
  link_type text,
  link_pos integer,
  primary key (link_src, link_tgt, link_type),
  FOREIGN KEY (link_src) REFERENCES nodes(node_id) ON DELETE CASCADE
);
")
          _ (ensure-today!)]
    (reset! db-init? true)
    (prn "cache init"))
  )


(defn pq [q & args]
  (p/let [r (apply db-exec q args)
          c (count r)]
    ;; (clojure.pprint/print-table r)
    (klog "qr" r c)
    r))

(comment

  (goog.string.format "some %s rest" "boo")

  (db-tick!)

  (pq "select * from files where file_name like '%2024-11-25%' limit 10")
  (cache-file-reindex!
   {:file_name "/roam/daily/2024-11-25.org",
    :file_modified_time "2024-11-26T04:23:19.000Z",
    :file_local_modified nil,
    :file_contents
    ":PROPERTIES:\n:ID: afaf31f2-dc1d-462e-98dd-bcfc1afbcf12\n:END:\n#+title: 2024-11-25\n\nok so now we have some shit actually.\n"})

  (pq "delete from files where file_name like '%2024-11-25%'")

  (pq "select * from nodes where node_file_name like '%2024-11-25%' limit 10")

  (datetime-str-local)

  (js/Date. "Sat, 02 Nov 2024 04:07:44 GMT")
  (prn "w")
  (re-matches #".*/" "/20210318212810-personal_projects/")

  (p/let [r
          (fileserver-get "/20210318212810-personal_projects.org")]
    (klog "got" r))

  (str/ends-with? "teac/" "/")

  (pq "select file_name from files where file_name like '%daily%' limit 10")

  (re-matches #"(?:^\s*$)|(?:#\+(\S+):(.*))"
              " ")

  (def x "
a
bc
c")

  (->> x
       (str/split-lines)
       (reductions (fn [[pc ps] s]
                     [(+ pc (count ps) 1) s])
                   [-1 ""])
       (drop 1))




  (pq "
select file_name
from files
where node_file_name like '%daily%'
order by file_name desc
limit 10
")
  ;; plan
  ;; list all files,
  ;; create local cache
  ;;

  (db-tick!)
  (pq "select * from nodes")
  (pq "
select * from nodes
join files f on node_file_name = file_name
where
node_start = 0
and node_file_name like '%/daily/%'
order by
node_file_name desc
")
  )

(def db-update-ref (rg/cursor state [:db-update-i]))

(defn db-tick! []
  (swap! db-update-ref inc))

(def top-query-ref (rg/cursor state [:top-query]))

(defn top-query-set! [q]
  (reset! top-query-ref q))

(def top-nodes-ref (rg/cursor state [:top-nodes]))

(def top-nodes-track
  (rg/track
   (let [last-query (atom nil)]
     (fn []
       (let [{:as   newq
              :keys [top-query db-update db-init? needs-update?]}
             (swap! last-query
                    (fn [oldq]
                      (let [newq {:top-query @top-query-ref
                                  :db-init?  @db-init?
                                  :db-update @db-update-ref}]
                        (klog "checking" newq oldq)
                        (assoc newq
                               :needs-update?
                               (not= newq (dissoc oldq :needs-update?))))))]
         (when (and db-init?
                    needs-update?)
           (klog "query nodes2" newq)
           (p/let [_ (ensure-today!)
                   nodes (me/match top-query
                           [:dailies-q]
                           (db-exec "
select * from nodes
join files f on node_file_name = file_name
where
node_start = 0
and node_file_name like '%/daily/%'
-- test stuff
-- and node_file_name like '%2024-10-03%'
-- and node_file_name like '%2024-11-12%'
order by
node_file_name desc
")
                           [:q-file ?file-name] (db-exec "

select * from files
join nodes on node_file_name = file_name
where file_name = :file_name
and node_start = 0
" {:file_name ?file-name})
                           [:q-node-id ?id]
                           (db-exec "
select * from nodes
join files on node_file_name = file_name
where node_id = :node_id
" {:node_id ?id})
                           ?x (klog "unknown query!" top-query))]
             (klog "finished query, resetting")
             (when (seq nodes)
               (reset! top-nodes-ref nodes)))))
       @top-nodes-ref))))

(def server-status-cursor (rg/cursor state [:server-status]))

(defn server-status-set! [{:keys [_status _msg] :as x}]
  (p/let [_ (p/delay 5000)]
    (swap! server-status-cursor (fn [{:as y :keys [status]}]
                                  (when (or (= :error status)
                                            (not= x y))
                                    y))))
  (reset! server-status-cursor x))

(defn server-status-disp []
  (let [{:keys [status msg]} @server-status-cursor]
    (when msg
      [:div {:style {:color       (if (= :error status)
                                    "red"
                                    "lightgreen")
                     :margin      2
                     :display     (if (= :error status)
                                    "block"
                                    "inline")
                     :font-family "monospace"
                     :white-space "pre-wrap"}}
       (if (string? msg)
         msg
         (-> msg
             (clojure.pprint/pprint)
             with-out-str))])))


(defn server-status-checkp [p]
  (-> p
      (p/then (fn [x]
                (when (map? x)
                  (server-status-set! x))
                x))
      (p/catch (fn [e]
                 (server-status-set! {:status :error
                                      :msg {:msg (ex-message e)
                                            :data (ex-data e)}})))))

(defn tlink-alias-title [title]
  (->> title
       (re-seq #"(\[\[)([^\[\]]+)(?:(\]\[)([^\[\]]+))?(\]\])")
       (reduce (fn [s [m pre id _mid alias post]]
                 (str/replace s
                              (re-pattern (u/re-quote m))
                              (or alias id)))
               title)))

(defn tlink-from-node [{:as x :keys [node_title node_id]}]
  (str "[[id:" node_id "][" (tlink-alias-title node_title) "]]"))

(comment

  (deep-merge {:a 'b :c {:d :e}} {:a 'c :c nil})

  ;; title
  ;; - if subnode_of property present, use that
  ;; - else, use header hierarchy, not incl file name
  (pq
"
select * from nodes where node_title like '%ideas for st%'
limit 10
"
   )
  (p/let [x (db-exec "
select *, json_extract(node_props, '$.search_text') as search_text
from nodes
where node_file_name like '%paul%'
limit 10
"
                   )]
    (klog "g" x))

  (pq "

select * from nodes
where json_extract(node_props, '$.search_text') like '%Crazy New Ideas%'
limit 10
")

  (pq "
with recursive nps as (
  select n.*, p.value pid, node_id root_id from nodes n
  join json_each(node_props, '$.cache_pids') p
  union
  select ps.*, pid, root_id
  from nodes ps
  join nps on nps.pid = ps.node_id
)
select * from nps
where
--json_extract(node_props, '$.search_text') like '%david%' limit 5
root_id = 'c21ebe89-5182-4fbd-87f9-ef5e4f14e420'
-- root_id = '6028f4ed-bcdf-40b8-8696-f3146ff83a31'
")
  "
"
  )

(defn search-node-candidates []
  (db-exec "select *, json_extract(node_props, '$.search_text') as search_text from nodes"))

;; TODO
;; - delete and reparse on save
;; - update db
;; HACK (s)
;; - save just the whole file and only display as whole file
(defn node-disp [{{:keys [file_contents] :as node+file} :node+file}]
  (rg/with-let [search-props (rg/atom {:search-enabled?   false
                                       :search-wrap-props {}})
                candidates (rg/atom nil)
                ed-atom    (atom nil)
                do-search! (fn [& {:keys [props]}]
                             (swap! search-props
                                    deep-merge
                                    {:search-enabled? true
                                     :search-wrap-props
                                     {:style {:top (str
                                                    "calc(1em + "
                                                    (-> (js/window.getSelection)
                                                        (.getRangeAt 0)
                                                        (.getBoundingClientRect)
                                                        (.-top)
                                                        (+ (.-scrollY js/window)))
                                                    "px)")}}}
                                    props))]
    [:div {:style {:border-top "1px solid grey"}}
     (let [{:keys [search-enabled? search-wrap-props]} @search-props]
       (when search-enabled?
         [:div (deep-merge
                {:style {:position   "absolute"
                         :background "black"
                         :z-index    5}}
                search-wrap-props)
          [searchbox {:candidates @candidates
                      :focus?     true
                      :on-blur    (fn [x]
                                    ;; TODO focus back on text (somehow?)
                                    ;; insert link text
                                    ;; - so in either case, need a reference to like the selected editor
                                    (swap! search-props merge {:search-enabled? false})
                                    (klog "blur" x))
                      :on-select  (fn [{:as x :keys [ncmd node_title node_id]}]
                                    (cond
                                      (and node_title node_id)
                                      (some-> @ed-atom
                                              (rich-org/ed-transf :tdelete-text
                                                                  {:unit "character" :distance 2 :reverse true})
                                              (rich-org/ed-transf :tinsert-text
                                                                  (tlink-from-node x))
                                              (rich-org/ed-focus))
                                      (= ncmd :create-node)
                                      (do
                                        (some-> @ed-atom
                                                (rich-org/ed-transf :tdelete-text
                                                                    {:unit "character" :distance 2 :reverse true})
                                                (rich-org/ed-create-node-at-point)
                                                (rich-org/ed-focus)))
                                      :else (klog "unknown candidate!" x)))}]]))
     ^{:key file_contents}
     [rich-org/editor {:text    file_contents
                        :ed-atom ed-atom
                        :on-evt  (fn [{:as x :keys [ed-evt ed-key-chord ed-link-text ed-text]}]
                                   (case ed-evt
                                     :word
                                     (case ed-key-chord
                                       ["n" "C"] (do (reset! candidates
                                                             [{:ncmd :create-node
                                                               :text "create node"}
                                                              {:ncmd :insert-link
                                                               :text "insert link"}])
                                                     (do-search!)
                                                     )
                                       ["l" "N"] (do
                                                   (p/let [nodes (search-node-candidates)]
                                                     (->> nodes
                                                          (mapv (fn [{:keys [node_title search_text] :as f}]
                                                                  (assoc f :text (tlink-alias-title search_text))))
                                                          (reset! candidates)))
                                                   (do-search!)
                                                   )
                                       (do
                                         ;; (klog "got chord" ed-key-chord)
                                         nil)
                                       )
                                     :ed-click-link
                                     (me/match ed-link-text
                                       (me/re #"id:(.+)" [?m ?id])
                                       (top-query-set! [:q-node-id ?id]))
                                     :change
                                     (p/let [newf (assoc node+file
                                                         :file_contents ed-text
                                                         :file_local_modified true)
                                             result (cache-file-reindex! newf)]
                                       ;; TODO reparse file?
                                       ;; (klog "saved" result)
                                       )

                                     (klog "no ed event handler" ed-evt
                                           ;; x node+file
                                           )
                                     ))}]]))


(declare buttons file-search)


(defn main-page []
  (rg/with-let [file-candidates (rg/atom nil)
                display (rg/atom :test2)]
    (react/useEffect (fn []
                       (cache-init!)
                       ;(login-init!)
                       (constantly nil)))
    [:<>
     (->> buttons
          (map-indexed (fn [i {:keys [s f]}]
                         ^{:key i}
                         [:button {:style    {:margin ".5ch"}
                                   :on-click f}
                          s])))
     [server-status-disp]
     ;; (when (or (-> @server-status-cursor :status (= :error))
     ;;           (-> @login-ref :user-pass (count) (< 2)))
     ;;   [login-form])
                                        ;#_
     [file-search {:on-select (fn [{:keys [file_contents file_name] :as s}]
                                (klog "to update query here" file_name s)
                                (top-query-set! [:q-file file_name]))}]
     [:div {:style {:max-width "70ch"}}
      (->> @top-nodes-track
           (drop 0)
           (take disp-max-files)
           (map-indexed (fn [i x]
                          ^{:key {:nid (:node_id x)}} [node-disp {:node+file x}])))]
     [:hr]
     [:div
      "end"]]))


(def buttons
  [{:s "dailies"
    :f (fn [_]
         (top-query-set! [:dailies-q]))}
   {:s "server save"
    :f (fn [_]
         (->
          (p/let [modified-files (cache-file-get-modified)
                  resp           (->> modified-files
                                      (map (fn [{:keys [file_name file_contents]}]
                                             (fileserver-save file_name file_contents)))
                                      (p/all))
                  _ (->> modified-files
                         (map (fn [f]
                                (cache-file-upsert! (assoc f :file_local_modified false))))
                         (p/all))]
            (db-tick!)
            {:msg (str "saved " (count resp) " files")})
          (server-status-checkp)))}
   {:s "server pull"
    :f (fn [_]
         (server-status-set! nil)
         (prn "pulling files")
         (-> (p/let [fl (fileserver-list)
                     _
                     (->> fl
                          (keep (fn [{:keys [href modified created]}]
                                  (when (str/ends-with? href ".org")
                                    (p/let [gf (fileserver-get href)]
                                      (cache-file-reindex!
                                       (merge
                                        {:file_name          href
                                         :file_modified_time modified}
                                        gf))))))
                          (p/all))]
               (db-tick!)
               {:msg "pulled all files"})
             (ptime)
             (server-status-checkp)))}
   {:s "index files"
    :f (fn [_]
         (-> (p/let [fs (db-exec "
select * from files
")
                     records (->> fs
                                  (map cache-file-reindex!)
                                  (p/all))
                     records (mapcat identity records)]
               (db-tick!)
               {:msg (str "updated records" (count records))})
             (ptime)
             (server-status-checkp))
         )}])

(defn file-search [{:keys [on-select]}]
  (rg/with-let [search? (rg/atom false)
                file-candidates (rg/atom nil)]
    [:div {:style {:margin "0.5ch"}
           :on-click (fn [x]
                       (reset! search? true)
                       (p/let [files (db-exec "select * from files")]
                         (->> files
                              (mapv (fn [{:keys [file_name] :as f}]
                                     (assoc f :text file_name)))
                              (reset! file-candidates)))
                       )}
     (if @search?
       [searchbox {:on-blur (fn [& _] (reset! search? false))
                   :on-select (fn [x]
                                (reset! search? false)
                                (on-select x))
                   :focus? true
                   :candidates @file-candidates}]
       [:button "file search"])]))

(defonce rroot (react-client/createRoot (js/document.querySelector "#app")))

(rg/set-default-compiler! (rg/create-compiler {:function-components true}))

(defn ^:dev/after-load render []
  (.render
   rroot
   (rg/as-element [main-page])))

(defn ^:export init []
  (render))
