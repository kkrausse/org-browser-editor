(ns org-browser-editor.org-parse
  (:require
   [meander.epsilon :as me]
   [clojure.string :as str]))

(defn parse-props [kvs]
  (into {}
    (keep (fn [[k v]]
           (when (and k v)
             [(-> k str/trim str/lower-case keyword)
              (-> v str/trim)])))
    kvs))

;; parse to blocks
(defn parse-blocks [numbered-lines]
  (me/find numbered-lines
    [[?start (me/re #"\s*:PROPERTIES:")] .
     [_ (me/re #"\s*:(.+):(.*)" !ps)] ...
     [?end (me/re #"\s*:END:\s*" ?ends)] .
     & ?post]
    [{:t     :property-drawer
      :start ?start
      :end   (+ ?end (count ?ends))
      :props (->> !ps
                  (map rest)
                  (parse-props))}
     ?post]

    [[?s (me/re #"#\+(\S+):(.*)" [?m ?k ?v])] & ?post]
    [{:t     :single-property
      :kvs   (parse-props [[?k ?v]])
      :start ?s
      :end   (+ ?s (count ?m))}
     ?post]

    [[?start (me/re #"(\*+ )(.+)" [?raw ?stars ?title])]
     & ?post]
    [{:t        :headline
      :headline ?title
      :raw      ?raw
      :level    (dec (count ?stars))
      :start    ?start
      :end      (+ ?start (count ?raw))}
     ?post]

    [[?start (me/re #"#\+begin_(\S+)(.*)" [?raw ?type ?args])] .
     (me/and [_ _]
             !contents) ...
     [?end (me/re #"#\+end_(\S+)(.*)" [?raw-end ?type _])] .
     & ?post]
    [{:t          :org-block
      :block-type ?type
      :start      ?start
      :end        (+ ?end (count ?raw-end))
      :contents   !contents}
     ?post]

    [?h & ?post]
    [?h ?post]))

(defn add-props [blocks]
  (me/find blocks
    [{:t  :headline
      :as ?hl} .
     & (me/and [{:t     :property-drawer
                 :props ?pdp
                 :as    ?pd}
         & _]
               ?post)]
    [(merge ?hl
            {:props ?pdp})
     ?post]

    [:start &
     (me/and ?post
             [{:t     :property-drawer
               :props ?pdp} .
              [_ (me/app str/trim "")] ... .
              {:t   :single-property
               :kvs !kvs} ...
              & (me/not [{:t :single-property} & _])])]
    [{:t          :file-props
      :file-props (merge ?pdp
                         (reduce merge {} !kvs))}
     ?post]

    [[(me/pred int? ?start) ?s] & ?post]
    [{:t     :text
      :start ?start
      :text  ?s
      :end   (+ ?start (count ?s))} ?post]

    [?h & ?post]
    [?h ?post]
    ))

(defn group-text [blocks]
  (me/rewrite blocks
    [{:t :text
      :text (me/and (me/not (me/re #"\s*" ))
                 !t)
      :start ?s}
     {:t :text
      :end ?end
      :text (me/and (me/not (me/re #"\s*" ))
                 !t)}
     & ?post]
    [nil
     [{:t :text
       :start ?s
       :text ~(str/join "" !t)
       :end ?end}
      &
      ?post]]

    [?h & ?post]
    [?h ?post]
    ))

(defn run-parse [pf line-seq]
  (loop [parsed   []
         to-parse line-seq]
    (if (seq to-parse)
      (let [[p ps] (pf to-parse)]
        (recur (conj parsed p)
               ps))
      parsed)))

(comment

  (run-parse
   group-text
             )

  (def x
    "a

b")

  (subs x 2)
  (pos-numbered-lines
   x)

  (->> (all-lines x)
       (partition-all 2)
       (map (fn [[x y]])))

  (def nx (list [0 "a"] [2 "c"] [3 "b"]))
  (->> nx
       (cons nil)
       (partition 2 1)
       (map (fn [[[p1 t1] [p2 t2]]]
              (if (and p1 t1)
                [(+ p1 (count t1) 1) t2]
                [p2 t2]))))

  )


(defn all-lines [s] (str/split s #"\n|\r\n" -1))

(defn plines-update-pos [p+lines]
  (->> p+lines
       (reductions (fn [[pc ps] [_ s]]
                     [(+ pc (count ps) 1) s])
                   [-1 ""])
       (drop 1)))

(defn pos-numbered-lines [text]
  (->> (all-lines text)
       (reductions (fn [[pc ps] s]
                     [(+ pc (count ps) 1) s])
                   [-1 ""])
       (drop 1)))

(defn annotate-hl-parents [blks]
  (->> blks
       (reductions (fn [{hls :hls} blk]
                     (if (-> blk :t (= :headline))
                       (let [big-hls (->> hls
                                          (filter (fn [{:keys [level]}]
                                                    (< level (:level blk))))
                                          vec )]
                         {:hls (conj big-hls blk)
                          :blk (assoc blk
                                      :hl-parents big-hls)})
                       {:hls hls
                        :blk (assoc blk :hl-parents hls)}))
                   {:hls []})
       (drop 1)
       (map :blk)))

(defn parse-lines [pos+lines]
  (->> pos+lines
       (into [:start])
       (run-parse parse-blocks)
       (run-parse add-props)))

(defn parse-org [& {:keys [text]}]
  (let [nlines   (pos-numbered-lines text)
        hl-parse (parse-lines nlines)
        headlines (->> hl-parse
                       (filter :headline))
        headlines (->> headlines
                       (annotate-hl-parents)
                       (map (fn [{:keys [level start props] :as x}]
                              (assoc x
                                     :id (get props :id)
                                     :end
                                     (->> headlines
                                          (filter (fn [{l2 :level
                                                        s2 :start}]
                                                    (and (< start s2)
                                                         (<= l2 level))))
                                          (first)
                                          :start)))))]


    {:file-props (->> hl-parse
                      (keep :file-props)
                      first)
     ;; #_#_#_#_
     :links (->> nlines
                 (mapcat (fn [[n s]]
                           (->> s
                                (re-seq #"(?:\[\[)id:([^\[\]]+)(?:\]\[([^\[\]]+))?\]\]")
                                (map (fn [[m id alias]]
                                       (let [pos (+ n (str/index-of s m))]
                                         {:id id
                                          :hl-src (->> headlines
                                                       (filter (fn [{:keys [start end id]}]
                                                                 (and (< start pos)
                                                                      id
                                                                      (or (nil? end)
                                                                          (< pos end)))))
                                                       (sort-by (comp #(* -1 %) :start))
                                                       (first)
                                                       :id)
                                          :pos pos
                                          :alias alias})))))))
     :headlines headlines
     :parse hl-parse}))

(comment

  (->> (re-seq #"(?:\[\[)id:([^\[\]]+)(?:\]\[([^\[\]]+))?\]\]"
           x)
      ; (map (fn [[a ]]))
       )
  (comment
    (["[[id:97d243ae-1c3c-4494-8fbd-552d584b049e]]"
      "id:97d243ae-1c3c-4494-8fbd-552d584b049e"
      nil]
     ["[[id:eef600cc-aecb-4c71-b33f-75a16fe1496d][notes app]]"
      "id:eef600cc-aecb-4c71-b33f-75a16fe1496d"
      "notes app"]))

  (def x "
** and another thing


with just random stuffs which links [[id:97d243ae-1c3c-4494-8fbd-552d584b049e]] so




* [[id:eef600cc-aecb-4c71-b33f-75a16fe1496d][notes app]]


trying to backport roam notes into new thing, or at least see how that would b


")
  )
