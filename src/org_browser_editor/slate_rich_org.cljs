(ns org-browser-editor.slate-rich-org
  (:require
   ["react" :as react]
   ["slate" :as slate]
   ["slate-react" :as slate-react]
   [clojure.pprint]
   [clojure.string :as str]
   [org-browser-editor.util :as cu :refer [ed-wo-norm gen->seq deep-merge]]
   [meander.epsilon :as me]
   [reagent.core :as rg]
   [org-browser-editor.org-parse :as org-parse]))

(def ^:dynamic *ed*)

(defn ->ed [] *ed*)

(defn klog [& args]
  (js/console.log args))

(defn simple-graph
  "takes the pattern of slate of {:t 'my-defined-type' :children [... {:text 'somfeting'}]}
  and returns [:my-defined-type {:path [path from root]} ...children]
  text nodes are just strings.
  `ignore-pad?` is if we should ignore empty strings. I think yes
  `include-path?` is if to include that map which includes path
  "
  [node & {:keys [include-text? ignore-pad? include-path? start-path]
           :or   {ignore-pad?   true
                  include-path? true
                  include-text? true}}]
  (let [n (js->clj node :keywordize-keys true)
        p (or (js->clj start-path) [])]
    (me/rewrite
      (assoc n :p p)
      {:t        ?t
       :p        ?p
       :props    ?props
       :children (me/and [!c ...]
                         (me/let [(!index ...) (range (count !c))]))
       & ?kvs}
      [~(keyword ?t) .
       & ~(if include-path? [(merge
                                        ;#_
                              (->> ?kvs
                                   ;; need to filter out
                                   (filter (fn [[k v]]
                                             ;; allowlist for user set keys
                                             (or (#{:is-inline :is-void
                                                    :el-path :void-text}
                                                  k)
                                                 (str/starts-with? (name k) "el-"))))
                                   (into {}))
                              ;; ?props
                              {:path ?p})] []) .
       & (me/app
          (fn [cs]
            (cond->> cs
              (and ignore-pad?
                   (some vector? cs)) (remove (fn [x]
                                                (or (= x "")
                                                    (and (vector? x)
                                                         (= :txt (get x 0))
                                                         (= "" (get x 2))))))))
          [(me/cata (me/app (fn [[i c]]
                              (assoc c :p (conj ?p i)))
                            [!index !c])) ...])]
      {:text ?t
       :p    ?p}
      ~(if include-text?
         (if include-path? [:txt {:path ?p} ?t] [:txt ?t])
         ?t)
      ?e [:error! ?e])))

(defn graph-at [ed p]
  (simple-graph (first (slate/Editor.node ed (clj->js p))) :start-path p))

;; sometimes you wanna remove padding retroactively
(defn simple-unpad [node]
  (if (vector? node)
    (->> node
         (mapcat (fn [x]
                   (cond
                     (= "" x) []
                     (vector? x) (or (and (vector? x)
                                          (= :txt (get x 0))
                                          (= "" (get x 2))
                                          [])
                                  [(simple-unpad x)])
                     :else [x])))
         (into []))
    node))

(defn expand-simple "
  inverts simple
  "
  [simple]
  (me/rewrite simple
    [?t (me/pred map? ?props) & [!c ...]]
    {:t ~(and ?t (name ?t)) :children [(me/cata !c) ...]
     & ?props}

    [?t & [!c ...]] {:t ~(and ?t (name ?t)) :children [(me/cata !c) ...]}

    (me/pred string? ?t) {:text ?t}))

(defn prn-simple [& {:as kwargs}]
  (let [graph
        (clojure.pprint/pprint (merge (js->clj kwargs)
                                      {:editor (simple-graph (->ed))}))]
    (klog graph kwargs)))

;;;; path stuff
(defn pchild [p n]
  (-> p js->clj (conj n) clj->js))

(defn pparent [p]
  (-> p
      js->clj
      (->> (butlast)
           (into []))))

(defn pnext [p]
  (-> p
      js->clj
      (->> (into []))
      (update (dec (count p)) inc)))

(defn pprev [p]
  (-> p
      js->clj
      (->> (into []))
      (update (dec (count p)) dec)))

(defn ed-pathfn [ed path]
  (let [pr (slate/Editor.pathRef ed (clj->js path))]
    (fn []
      (vec
       (.-current pr)))))

(defn ed-transf [ed op & args]
  (let [ops {:tset slate/Transforms.setNodes
             :tsplit slate/Transforms.splitNodes
             :twrap slate/Transforms.wrapNodes
             :tunwrap slate/Transforms.unwrapNodes
             :tlift slate/Transforms.liftNodes
             :tinsert slate/Transforms.insertNodes
             :tmove slate/Transforms.moveNodes
             :tremove slate/Transforms.removeNodes
             :tinsert-text slate/Transforms.insertText
             :tdelete-text slate/Transforms.delete
             :tselect slate/Transforms.select
             :tdeselect slate/Transforms.deselect
             :tmerge slate/Transforms.mergeNodes
             :tset-point slate/Transforms.setPoint
             :tmove-point slate/Transforms.move}]
    (if-let [f (get ops op)]
      (apply f ed (clj->js args))
      (prn "op not found!" op))
    ed))

  (defn ed-nest-set "
  regular slate setNodes won't set children. this does.
  takes a slate definition
  "
    {:style/indent :defn}
    [ed to-set path]
    (let [path (vec path)
          [n _p] (slate/Editor.node ed (clj->js path))
          to-set (->> to-set
                      (map (fn [[k v]]
                             [k (if (fn? v)
                                  (v (aget n (clj->js k)))
                                  v)]))
                      (into {}))]
      (ed-wo-norm ed
                  (ed-transf ed :tset
                             (dissoc to-set :children)
                             {:at path})
                  (doseq [[i c] (map-indexed vector (:children to-set))]
                    (ed-nest-set ed c (conj path i))))
      ed))

  (defn ed-parse-re "
  splits a regexp that should produce from re-matches [_str captures..]
  into separate nodes for each capture
  `path` is the text path
  "
    [ed path match labels]
    (let [match  (mapv (fnil identity "") match)
          ->p    (ed-pathfn ed path)
          strlen (count (first match))]
      (ed-wo-norm ed
                  (-> ed
                      (ed-transf :twrap
                                 {:t :tmp}
                                 {:at (->p)})
                      (ed-transf :twrap
                                 {:t :tmp}
                                 {:at (->p)}))
                  (loop [p        (pparent (->p))
                         tl       0
                         [s & ss] (rest match)]
                    #_
                    (klog "splitting or adding"
                          p
                          s
                          (simple-graph ed))
                    (when s
                      (let [cl (count s)]
                        (cond
                          (and (< (+ cl tl) strlen)
                               (pos? (count s)))
                          (ed-transf ed
                                     :tsplit
                                     {:at {:path   (pchild p 0)
                                           :offset cl}})
                          (zero? (count s))
                          (do
                              (ed-transf ed
                                         :tinsert
                                         {:t :tmp
                                          :children [{:text ""}]}
                                         {:at p})))
                        (recur (pnext p)
                               (+ tl cl)
                               ss))))
                  (ed-nest-set
                    ed
                    {:children (->> labels
                                    (map (fn [l] {:t l
                                                  :is-inline true}))
                                    (vec))}
                    (drop-last 2 (->p))))
      ed))

(defn ed->text [ed]
  (-> ed
      (js->clj :keywordize-keys true)
      ;; ((fn [x] (klog "text ed" ed) x))
      (me/rewrite
       {:is-void true
        :is-inline ?inline
        :void-text ?t}
       {:is-inline ?inline :s ?t}

       {:is-inline ?inline
        :children [!cs ...]}
       {:is-inline ?inline
        :s
        (me/app (fn [cs]
                  (str/join (if (some :is-inline cs) "" "\n")
                            (map :s cs)))
                [(me/cata !cs) ...])}

       {:text (me/some ?t)} {:is-inline true :s ?t}

       ?x ~(klog "failed text" ?x)
       )
      :s))

(defn ed->text- [ed]
  (klog "ed-t" ;(js->clj ed :keywordize-keys true)
        (ed->text- ed))
  (str/trim
   (me/rewrite
    (js->clj ed :keywordize-keys true)

    {:is-inline ?inline
     :children (me/some ?cs)}
    (me/app (fn [s]
              (str s (if ?inline "" "\n")))
            (me/cata ?cs))


    {:text (me/some ?t)} ?t

    [!c ...]
    (me/app #(str/join "" %)
            [(me/cata !c) ...])

    ?x ~(prn "miss" ?x))))

;; when calculating total offset, how do i ensure corresponds to ed-text
;; utils for like getting context idk if needed
(defn ed-selection [ed]
  (let [selection (.-selection ed)
        positions (->> (slate/Editor.positions
                        ed
                        (clj->js
                         {:at (second (slate/Editor.first ed []))
                          :unit "character"
                          :voids true}))
                       (cu/gen->seq))
        total-offset (when selection
                       (-> ed
                        (slate/Node.fragment
                                             (clj->js {:anchor (first positions)
                                                       :focus  (.-anchor selection)}))
                        (->> (hash-map :children))
                        (ed->text)
                        (count)))]

    (some-> selection
            (js->clj :keywordize-keys true)
            (assoc :total-offset total-offset))))

(defn dfs-nodes [ed & [p]]
  (let [[n p'] (slate/Editor.node ed (clj->js (or p [])))]
    (if-not (.-children n)
      [[n p]]
      (conj (->> (slate/Node.children ed p')
                 (gen->seq)
                 (mapcat (fn [[_ p]]
                           (dfs-nodes ed p)))
                 (into []))
            (clj->js [n p'])))))

(defn simple-children [snode]
  (if (map? (get snode 1))
    (subvec snode 2)
    (subvec snode 1)))

(defn simple-get [sn p]
  (me/rewrite [sn p]
    [?sn []] ?sn
    [?sn [?p & ?np]]
    (me/cata
     [~(get (simple-children ?sn)
            ?p)
      ?np])))

(defn simple-nodes-to-root [ed & {:keys [path ignore-pad? include-path?]
                                  :or {ignore-pad? true}}]
  (let [sg (simple-graph ed :ignore-pad? false :include-path? include-path?)
        p (or path
              (-> ed (ed-selection) :focus :path))]
    (when p
      (->> (range (inc (count p)))
           (map #(subvec p 0 %))
           (reverse)
           (mapv (partial simple-get sg))
           ((fn [xs] (cond->> xs
                       ignore-pad? (mapv simple-unpad))))))))

  (defn ->add-classes-fn "
  returns a function that caches the previous time it was run, adds classes
  "
    [ed]
    (let [prev* (atom nil)]
      (fn []
        (let [graph (simple-graph ed)
              w-classes
              (cu/add-classes graph
                [:hl-hl _ [:hl-dots ?dots] & _]
                [~(str "hl-level-" (count ?dots))]
                [:indent _ [:indent-space ?dash] & _]
                [~(str "dash-level-" (int (/ (inc (count ?dash)) 2)))]
                )]
          (when-not (= @prev* w-classes)
            (reset! prev* w-classes)
            (ed-nest-set ed
              (dissoc (expand-simple w-classes)
                      :t)
              []))))))

(defn text->ed-value [text]
  (->> (str/split-lines text)
       (map (fn [s]
              [:p s]))
       (into [nil])
       expand-simple
       :children
       clj->js))

(defn ed-prn [e msg & args]
  (let [ed (.-children e)]
    (klog msg (simple-graph e) args))
  e)

(comment

  (def x [:pdrawer
          {:path []}
          [:p {:path [0]} [:txt {:path [0 0]} ":PROPERTIES:"]]
          [:p
           {:path [1]}
           [:txt
            {:path [1 0]}
            ":ID:       e9e19bb0-94a0-4554-b7e2-0efe1bb749b5"]]
          [:p {:path [2]} [:txt {:path [2 0]} ":END"]]])

  (me/find x
           [:pdrawer {:path ?p} .
            &
            (me/not [[:p _ [:txt _ (me/re #":PROPERTIES:")]] .
                     [:p & _] ...
                     [:p _ [:txt _ (me/re #":END:")]]
                     ])
            ]
           :yes
           _ :no)

  (slate/createEditor )

  )
(defn ed-node+path [ed p]
  (slate/Editor.node ed (clj->js p)))

(def ^:dynamic *norm-default* true)
;; tk
(defn with-rich-org [ed]
  (let [default     ed.normalizeNode
        add-classes (->add-classes-fn ed)]

    (aset ed
          "isInline"
          (fn [el]
            (boolean (aget el "is-inline"))))
    (aset ed "defaultNormf" default)
    (aset
     ed
     "normalizeNode"
     (fn org-normalize [[node path]]
       (letfn [(unset-t [& {:keys [loc unwrap?]}]
                 (let [[n p] (slate/Editor.node ed (clj->js (or loc path)))]
                   ;; (ed-prn ed "pre unset" {:p p :loc loc :n (simple-graph n)})
                   ;; (klog "pre  unset " loc
                   ;;                     (simple-graph n :start-path path))
                   ;; have to kill void shit first
                   ;; (klog "unwrap" (.-t n))
                   (ed-transf ed :tset
                              {:is-void nil}
                              {:at p})
                   (if (or (.-text n)
                           unwrap?
                           (.isInline ed n))
                     (ed-transf ed
                                :tunwrap
                                {:at p})
                     (ed-transf ed :tset
                                {:t         "p"
                                 :classes   nil
                                 :style     nil
                                 :props     nil
                                 :is-inline nil
                                 :el-path   nil}
                                {:at p}))
                   #_
                   (klog "post unset" {:loc        loc
                                            :post-thing (simple-graph (first (slate/Editor.node ed (clj->js p))) :start-path path)
                                            :pre-unset  (simple-graph n :start-path path)})
                   ))]
         (let [[node _] (slate/Editor.node ed path)]
           #_
           (klog "norming at"
                                        ;path
                 #_
                 (simple-graph node :start-path (js->clj path)))
           (me/find
            (simple-graph node :start-path (js->clj path))

            ;; testing
            [:txt _ (me/re #".*\[test\].*")]
            (ed-prn ed "just testing"
                    {:totext (ed->text ed)
                     :full   (.-children ed)})

             ;; path is changing
             ;; idk why?
             ;; add el path for unique setting in render
             (me/and [(me/not :txt) {:path    ?p
                                     :el-path (me/not ?p)} & _]
                     ?x)
             (do
               ;; (klog "add path" ?p
               ;;       ;;?x
               ;;       path)
               (ed-transf ed
                          :tset
                          {:el-path ?p
                           :el-id (cu/->uuid)}
                          {:at ?p}))


             [:tmp {:path ?path} . _ ...]
            (unset-t :loc ?path :unwrap? true)

            ;; unwrap p's
             [:p {:path _} [:p {:path ?p} & _]]
            (unset-t :loc ?p :unwrap? true)


          ;;;;;;;;;;;;;;;;;;;;;;;;; headline
            ;; kill hl-hl
            [:hl-hl _ & (me/not [[:hl-dots & _] [:hl-title & _]])]
            (do
              #_
              (klog "killing hl"
                    (simple-graph ed)
                    (simple-graph node :start-path path))
              (unset-t))
             [(me/not :hl-hl) . _ ... [:hl-dots {:path ?p} _] . _ ...]
            (unset-t :loc ?p)

            ;; I think problem was hl-title wasn't dying
            ;; NOTE bad perf here
            ;; (me/$ [(me/not :hl-hl) . _ ... [:hl-title {:path ?p} & _] . _ ...])
            ;; (do (prn "unset title")
            ;;     (unset-t :loc ?p))

             [(me/not :hl-hl) . _ ... [:hl-title {:path ?p} & _] . _ ...]
            (do (prn "unset title")
                (unset-t :loc ?p))
            [:hl-dots _ & (me/not [[:txt _ (me/re #"\*+ ")]])]
            (unset-t)

             [:p {:path ?tp}
              [:txt {:path ?txtp} (me/re #"(\*+ )(.*)" [_ ?dots ?post-dot :as ?dot-match])]
             & _]
            (do
              ;; (klog "possible headline" ?dot-match ?tp ?txtp)
              (ed-wo-norm
               ed
               (-> ed
                   (ed-parse-re ?txtp ?dot-match ["hl-dots" "tmp"])
                   ;; (ed-prn "post parse")
                   (ed-transf :tlift
                              {;:split true
                               :at (pchild ?txtp 0)
                               :to ?txtp})
                   (ed-transf :twrap
                              {:t :hl-hl :is-inline true}
                              {:at ?tp})
                   ;; (ed-prn "post wrap")
                   (ed-transf :tmove
                              {;:split true
                               :at (me/match (graph-at ed ?tp)
                                     (me/$ [:hl-dots {:path ?p} & _]) ?p)
                               :to (me/match (graph-at ed ?tp)
                                     (me/$ [:p {:path ?p} & _]) ?p)
                               })
                   ;; (ed-prn "post unwrap --p should be head now?")
                   (ed-nest-set {:t         :hl-title
                                 :is-inline true
                                 :children  [{:is-inline true}]}
                                (pchild ?tp 1))
                   (ed-transf :twrap
                              {:t :p}
                              {:at ?tp})
                   ;; (ed-prn "post set")
                   )
               )
              ;; (ed-prn ed "post wo norm?" (.-children ed))
              )



            ;; links
            ;; why the ...?
             [:txt {:path ?path} (me/re #"(.*)(\[\[)([^\[\]]+)(?:(\]\[)([^\[\]]+))?(\]\])(.*)"
                                       [_all _pre _l ?link ?alias-split ?alias _r _post :as ?match])]
            (do
              (ed-wo-norm
               ed
               #_
               (ed-prn ed "pre parse link" {:p  ?path
                                            :n  (simple-graph node)
                                            :n2 (slate/Editor.node ed path)})
               (ed-parse-re ed ?path ?match ["tmp" "link-l" "id-link-id" "link-alias-split" "link-alias" "link-r" "tmp"])
               (let [pfn (ed-pathfn ed ?path)
                     has-alias? (some? ?alias)]
                 (-> ed
                     ;; (ed-prn "pre wrap")
                     (ed-transf :tlift
                                {:at    (pchild (pfn) 0)
                                 :split true})
                     (ed-transf :tlift
                                {:at    (pchild (pfn) 5)
                                 :split true})
                     ;; (ed-prn "pre del")
                     ;; delete id link data
                     (cond->
                         has-alias?
                       (ed-transf :tdelete-text
                                  {:at   (-> (pfn) (pchild 1) (pchild 0))
                                   :unit "block"})
                       ;; delete split thing
                         has-alias?
                       (ed-transf :tdelete-text
                                  {:at   (-> (pfn) (pchild 2) (pchild 0))
                                   :unit "block"})
                       )
                     ;; (ed-prn "post del del")
                     (ed-nest-set {:is-inline true
                                   :t         :id-link
                                   :props     {:link-id ?link :el-inline true}
                                   :children  [{}
                                               {:props   {:link-has-alias (some? ?alias)
                                                          :is-void        (some? ?alias)
                                                          :link-id        ?link}
                                                :link-text      ?link
                                                :void-text      ?link
                                                :is-void (some? ?alias)}
                                               (if has-alias?
                                                 {:is-void true
                                                  :void-text "]["}
                                                 {})]}
                                  (pfn))
                     ;; (ed-prn "parsed id link" {:ed-text (ed->text ed)})
                     )

                 ))
              ;; (ed-prn ed "bad") [[a][b]]
              ;; (klog "also " (.-children ed))
              )

            ;; kill link (in all possible ways lol)

            [:id-link _ & (me/not (me/or [[:link-l & _] [:id-link-id & _] [:link-r & _]]
                                         [[:link-l & _] [:id-link-id & _] [:link-alias-split & _] [:link-alias & _] [:link-r & _]]))]
            (unset-t)
             [(me/not :id-link) . _ ... [:id-link-id {:path ?p :void-text ?lt} & _] . _ ...]
            (do
              ;; (ed-prn ed "killing id link at" ?p)
              ;; NOTE void manually inserted?
              (ed-transf ed
                         :tinsert-text
                         ?lt
                         {:at ?p})
              (unset-t :loc ?p))
             [(me/not :id-link) . _ ... [:link-r {:path ?p} & _] . _ ...]
            (unset-t :loc ?p)
             [(me/not :id-link) . _ ... [:link-l {:path ?p} & _] . _ ...]
            (unset-t :loc ?p)
             [(me/not :id-link) . _ ... [:link-alias {:path ?p} & _] . _ ...]
            (unset-t :loc ?p)
             [(me/not :id-link) . _ ... [:link-alias-split {:path ?p :void-text ?vt} & _] . _ ...]
             (do
               (ed-transf ed
                         :tinsert-text
                         ?vt
                         {:at ?p})
               (unset-t :loc ?p))
            [:link-l _ (me/not [:txt _ "[["])]
            (unset-t)
            [:link-r _ (me/not [:txt _ "]]"])]
            (unset-t)
            [:id-link-id _ (me/not [:txt _ (me/re #"[^\[\]]*")])]
            (unset-t)

             ;; bullets
             ;; ok so need to keep everything as like whatever?
             [:p {}
              [:txt {:path ?tp} (me/re #"(- )(.*)" ?m)] & _]
             (-> ed
                 (ed-parse-re ?tp ?m ["bullet" "tmp"]))

             [:p {}
              [:leading-space & _]
              [:txt {:path ?tp} (me/re #"(- )(.*)" ?m)] & _]
             (-> ed
                 (ed-parse-re ?tp ?m ["bullet" "tmp"]))

             ;; kill
             [:bullet {:path ?p} & (me/not [[:txt _ (me/re #"(- )")]])]
             (unset-t :loc ?p)

            ;; indent
            ;; tbh maybe the text wrap thing should be handled above
            ;; trying to set the offset, etc might be easier but you'd have to do at the higher level...
             [:p {:path ?pp}
              [:txt {:path ?ip} (me/re #"(\s+)(.*)" ?m)]
              & (me/not
                 [[:txt _ (me/re #"(\s+)(.*)")]
                  & _])]
             (do
               ;; what
               ;; (klog "making indent"
               ;;       (simple-graph (first (ed-node+path ed ?pp))))
               (-> ed
                   (ed-parse-re ?ip ?m ["leading-space" "tmp"])
                   ))

             ;; kill to suck up more space if available
             [:p {:path ?p} [:leading-space {:path ?sp} _] [:txt {:path ?tp} (me/re #"\s+.*")] & _]
            (do (unset-t :loc ?sp :unwrap? true)
                (-> ed
                    (ed-transf :tmerge {:at (pnext ?sp)}))
                ;; (klog "merged text stuffs" ?tp ?sp ?p
                ;;       (pnext ?sp)
                ;;       (simple-graph (first (ed-node+path ed ?p)) :start-path ?p)
                ;;       (first (ed-node+path ed ?p)))
                ;; then merge but not working
                )

             [:leading-space {:path ?p} & (me/not [[:txt _ (me/re #"\s+")]])]
            (unset-t :loc ?p)

            ?x (do
                 #_
                 (klog "pret default" ?x)

                 (when *norm-default*
                   (default [node path]))
                 #_
                 (klog "post default"
                       (.-children ed)
                       #_
                       (-> (slate/Editor.node ed path)
                           first
                           (simple-graph :start-path path)))
                 ))))))
    ed))

(comment

  (re-matches #"([-|\*] )(.+)" "* taco")
  (macroexpand '(me/find 'x
                         [:a _ ... 'b] 'c))

  (do
    (ed-prn @test-ed "wat")
    nil)
  ;; seems like the regexp thing does so many matches and unmatches that its like terrible.
  ;; ideal would be like
  ;; - parse as a string
  ;; - divide / dileaneate however
  ;; - hide some nodes
  ;;   - can automatically convert from one to the other bc everything is text based, not tree based.

  (conj [] 1)
  )


(declare test-vals)

(def test-ed (atom nil))

(defn el->styling [el parse]
  (let [extra-attrs [{:class (-> el :t)}
                     {:class (if (:is-inline el) "el-inline" "el-block")}
                     {:style {"--start-indent" (-> parse :indent-level)
                              "--org-is-bullet" (if (-> parse :t (= :bullet-item)) "1" "0")
                              "--bullet-parent-indent" (-> parse :bullet-parent :indent-level)
                              }}]
        attrs
        (me/find (simple-graph el)
          [:p _
           [:bullet & _]
           & _]
          {:style {"--line-indent" 1}
           :class "indent-line"
           }
          [:p _
           [:leading-space _
            [:txt _ (me/app count ?c)]]
           [:bullet & _]
           & _]
          {:style {"--line-indent" (+ 1 ?c)
                   "--has-bullet" "1"}
           :class "indent-line"
           }

          [:p _ [:leading-space _
                 [:txt _ (me/app count ?c)]]
           & _]
          {:style {"--line-indent" ?c}
           :class "indent-line" ;; (str "indent-line-" ?c)
           })]
    (->> [attrs]
         (concat extra-attrs)
         (reduce (fn [m {:keys [style class]}]
                   (-> m
                       (update :style merge style)
                       (update :classes conj class)))
                 {:style {}
                  :classes []}))))

(defn ed-focus [ed]
  (slate-react/ReactEditor.focus ed))

(comment

  (slate/createEditor)
  (let [text "
- [[a][b]]
"]

    (->> text
         (org-parse/all-lines)
         (map text->pre-norm-ev)
         (js->clj))
    )




  ;; why is normalization not scaling linearly with size of editor
  ;;
  )

(defn pre-norm-ev-1 [text]
  (let [ed (with-rich-org (slate/createEditor))]
      (aset ed "children"
            (text->ed-value text))
      ;; (->> ed (dfs-nodes) (map second) (klog "paths"))
      (->> ed
           (dfs-nodes)
           ;; (reverse)
           (run! (fn [[node path]]
                   (binding [*norm-default* false]
                     (when (slate/Node.has ed path)
                       (ed.normalizeNode [node path])))))
           (ed-wo-norm ed)
           )
      (-> ed
          (.-children))))

;; HACK I have no idea why slate normalizing doesn't scale linearly but this is way faster
(defn text->pre-norm-ev [text]
  (->> text
       (org-parse/all-lines)
       (map pre-norm-ev-1)
       (reduce (fn [a c]
                 (.concat a c))
               (clj->js []))
       (time)))


;; probably the iterative thing?
;; try to annotate
;; if i do org parse, then
;; OR we use the org-parse thing
;;    then we creae conformant lines
;;
;; nah that seems too weird just go from scratch
;; do we need to go any deeper than the line though?
;; -- no?
;; - iterate through, spit out path annotations as data,
;;   update data after
(defn simple->text [xs]
  (ed->text (expand-simple (into [nil {:is-inline true}] xs))))

(defn ed-macro-parse-1 [lines]
  ;; NOTE passing in no text parse
  (me/find
   lines
   [[:p {:path ?p :el-id !eid} (me/re #"\s*:PROPERTIES:\s*")] .
    [:p {:path !pn :el-id !eid}
     & (me/app
        simple->text
        (me/re #"\s*:(.+):(.*)" !ps))] ...
    [:p {:path ?pe :el-id !eid} (me/re #"\s*:END:\s*" ?ends)] .
    & ?post]
   [{:t :property-drawer
     :props (->> !ps (map rest) (org-parse/parse-props))
     :paths (into [?p ?pe] !pn)
     :el-ids !eid}
    ?post]


   [[:p {:path ?p :el-id !eid} (me/re #"#\+begin_(\S+)(.*)" [?raw ?type ?args])] .
    [:p {:path !pn :el-id !ceid}
     & _] ...
    [:p {:path ?pe :el-id !eid} (me/re #"#\+end_(\S+)(.*)" [?raw-end ?type _])] .
    & ?post]
   [[{:t :org-typed-block
      :css-classes [(str ?type "-org-block")]
      :paths [?p ?pe]
      :el-ids !eid}
     {:t :org-typed-block-content
      :css-classes [(str ?type "-org-block")]
      :paths !pn
      :el-ids !ceid}]
    ?post]

   (me/and
    [[:p {:path ?p :el-id !eid} .
      [:leading-space _ (me/app count !indent)] ...
      [:bullet _ !txt] ...
      & [(me/not (me/or [:leading-space & _]
                        [:bullet & _]))
         & _]]
     & ?post]
    (me/guard (some? (first !txt))))
   [{:t :bullet-item
     :paths [?p]
     :el-ids !eid
     :indent-level (or (first !indent) 0)}
    ?post]

   [[:p {:path ?p :el-id !eid}
     [:leading-space _ (me/app count ?indent)] & _]
    & ?post]
   [{:t :indent-text
     :paths [?p]
     :el-ids !eid
     :indent-level ?indent}
    ?post]

   [[:p {:path ?p }
     [:hl-hl {:el-id !eid}
      [:hl-dots _ ?dots]
      [:hl-title _ & (me/app simple->text
                             ?title)]]]
    & ?post]
   [{:t :headline
     :el-ids !eid
     :level (-> ?dots count (dec))}
    ?post]

   ;; base case
   [[:p {:path ?p :el-id !eid} & _] & ?post]
   [{:paths [?p]
     :el-ids !eid}
    ?post]
   ))

;; I think we want the parse blocks to be in react state space
;; so can modify it
;; then we need to look up state for a given path.
;; -- normalize to include this
;;
(defn run-parse [pf line-seq]
  (loop [parsed   []
         to-parse line-seq]
    (if (seq to-parse)
      (let [[p ps] (pf to-parse)]
        (recur (or (if (vector? p)
                     (into parsed p)
                     (some->> p (conj parsed)))
                parsed)
               ps))
      parsed)))


(defn annotate-block-parents [blks]
  (->> blks
       (reductions (fn [{hls :hls buls :bullets} blk]
                     (if (-> blk :t (= :headline))
                       (let [big-hls (->> hls
                                          (filter (fn [{:keys [level]}]
                                                    (< level (:level blk))))
                                          vec )]
                         {:hls (conj big-hls blk)
                          :blk (assoc blk :hl-parent (last big-hls))
                          :bullets []})
                       (if (-> blk :t (#{:bullet-item :indent-text}))
                         (let [big-buls (->> buls
                                             (filter (fn [{:keys [indent-level]}]
                                                       (< indent-level (:indent-level blk))))
                                             vec)]
                           {:hls hls
                            :bullets (if (-> blk :t (= :bullet-item))
                                       (conj big-buls blk)
                                       buls)
                            :blk (-> blk
                                   (assoc :hl-parent (last hls))
                                   (assoc :bullet-parent (last big-buls))
                                   )})
                         {:hls hls
                          :blk (-> blk
                                   (assoc :hl-parent (last hls))
                                   ;; not bothering with regular indent lines rn
                                   )})))
                   {:hls []
                    :bullets []})
       (drop 1)
       (map :blk)))

;; OK so we we need
;; - update on subsequent parses
;; - maintain and update state based on options
;;
;; - how to maintain state between the parses?
;;   - parse has an "anchor" line. this line has an id.
;;     - id only changes when line changes
;;   - problem with anchor is like deleting state updates
;; - could try to keep state in slate?
;;   - but then we'd probably also want like rendering to be in slate
;;   - i don't trust slate bc its slow as fudge
(defn macro-parse-lines [ed]
  (->> (subvec (simple-graph ed :include-text? false)
               2)
       (run-parse ed-macro-parse-1)
       (annotate-block-parents)
       ;; make sure parents get cleared out with deep-merge
       ;; (map #(update % :hl-parent (fnil identity {})))
       ))

(def ted (atom nil))

(comment
  (ed->text)


  (do @ted)
  (-> @ted
      (ed-prn "te"))

  (->> @ted
      (macro-parse-lines)
      ;; (org-parse/annotate-block-parents)
      (klog "parse"))

  (let [ed @ted]
    (me/find (simple-nodes-to-root ed :include-path? true)
      (me/scan [:p {:path ?p}
                & _
                ])
      (ed-transf ed
                 :tinsert
                 (text->ed-value
                  (str ":PROPERTIES:\n" ":ID: testo" "\n:END:"))
                 {:at (pnext ?p)}))
    )

  (me/find {:a :b}
    {:c (me/some ?c)} true)
  )

(defn ed-create-node-at-point [ed]
  (me/find (simple-nodes-to-root ed :include-path? true)
      (me/scan [:p {:path ?p}
                & _])
      (ed-transf ed
                 :tinsert
                 (text->ed-value
                  (str ":PROPERTIES:\n" ":ID: " (cu/->uuid) "\n:END:"))
                 {:at (pnext ?p)})))

;; NOTE:
;; - this does (not?) is the id changing?

(defn slate-element [{:keys [slate-props on-evt parsed]}]
  (rg/with-let []
    (let [{:as el :keys [el-id]}  (js->clj (.-element slate-props) :keywordize-keys true)
          parse-id                (rg/track
                                   (fn []
                                     (->> @parsed
                                          :macro-parse
                                          (vals)
                                          (filter (fn [{:keys [el-ids]}]
                                                    (some #{el-id} el-ids)))
                                          first
                                          :el-ids
                                          first)))
          statec                  (rg/cursor parsed [:state @parse-id])
          parsec                  (rg/cursor parsed [:macro-parse @parse-id])
          {:as fp :keys [el-ids]} @parsec
          is-anchor?              (= el-id (first el-ids))
          fp                      (assoc fp :is-anchor? is-anchor?)
          pclasses                (->> (let [{:keys [t css-classes]} fp]
                                         (when t
                                           (concat
                                            [(name t)
                                             (when is-anchor? (str (name t) "-anchor"))]
                                            css-classes)))
                                       flatten
                                       (filter some?))
          collapsedc             (rg/cursor statec [:collapsed?])
          ;; set default collapsed
          _                       (when (-> fp :t (= :property-drawer))
                                      (swap! collapsedc (fnil identity true)))
          collapse-body?          (and (me/find fp
                                         {:t          :property-drawer
                                          :is-anchor? false} true)
                                       @collapsedc)
          collapse-head?          (and (me/find fp
                                     {:t          :property-drawer
                                      :is-anchor? true}
                                     true)
                                       @collapsedc)
          {el-style   :style
           el-classes :classes}   (el->styling el fp)]
      ;; NOTE
      ;; not rerendering when parse changes? idk why
      ;; (klog "render el "
      ;;       @parse-id
      ;;       @statec)
      [:div (deep-merge
             {:class    (->> el-classes
                             (concat pclasses)
                             (str/join " "))
              :style    (merge {;;:background (or (.. props -element -color) "inherit")
                                ;; :display (if (:is-inline el)
                                ;;            "inline"
                                ;;            "block")
                                }
                               (when-let [parent-lvl (-> fp :hl-parent :level)]
                                 {:margin-left (str (* parent-lvl 1) "ch")})
                               (when collapse-body? {:display "none"})
                               el-style
                               (js->clj (.. slate-props -element -style)))
              :on-click (fn [e]
                          (me/match [el fp]
                            [_ {:t          :property-drawer
                                :is-anchor? true}]
                            (do
                              (swap! statec update :collapsed? not))

                            [{:t "id-link"} _]
                            (do
                              (.preventDefault e)
                              (on-evt {:ed-evt       :ed-click-link
                                       :ed-link-text (me/match (:children el)
                                                       (me/$ {:t         "id-link-id"
                                                              :link-text ?t}) ?t
                                                       _ (klog "non-id-link" (:children el)))}))
                            _ (klog "no el evt"
                                    ;; el fp
                                    )))}
             (js->clj (.-attributes slate-props))
             )
       (.-children slate-props)
       (when collapse-head? "...")]
      )))

(defn js-defer [f]
  (js/setTimeout f 50))

;; editor autocompletes
(defn ed-handle-key-chord [ed chord js-event]
  (let [ntr (simple-nodes-to-root ed :ignore-pad? true :include-path? true)]
    (me/find [chord ntr]
             [[_ ... "Enter"]
              (me/scan [:p {:path _} .
                        [:leading-space _ [:txt _ !txt]] ...
                        [:bullet _ [:txt _ !txt]] ...
                        & [(me/not (me/or [:leading-space & _]
                                          [:bullet & _]))
                           & _]])
              ]
             (do
               (.preventDefault js-event)
               ;; do default stuffs
               (.insertBreak ed)
               (ed-transf ed :tinsert-text (str/join "" !txt)))
             [["Enter" " "]
              (me/scan [:p {:path _} .
                        [:leading-space _ [:txt _ !txt]] ...
                        [:bullet _ [:txt {:path !bpath} !txt]] ...
                        & (me/or [(me/not (me/or [:leading-space & _]
                                           [:bullet & _]))
                            & _]
                                 []
                                 nil)])
              ]
             (do
               (when-let [p (first !bpath)]
                 (.preventDefault js-event)
                 (ed-transf ed :tinsert-text
                            " "
                            {:at {:path p
                                  :offset 0}}))
               )
             [?x ?y] (do
                       ;; (klog "missed" ?x (take 3 ?y))
                         nil))))

(comment

  (def x [[:txt {:path [10 2]} "taco"]
          [:p
           {:el-path [0],
            :el-id "04e5d4f2-9935-40bc-a9f6-ef479b780217",
            :path [10]}
           [:bullet
            {:is-inline true,
             :el-path [0 0],
             :el-id "0cfac4fb-13ee-43b0-b42d-cf8da9a32f20",
             :path [10 1]}
            [:txt {:path [10 1 0]} "- "]]
           [:txt {:path [10 2]} "taco"]]
          ])
  (me/find x
    (me/scan [:p {:path _} .
              [:leading-space _ [:txt _ !txt]] ...
              [:bullet _ [:txt _ !txt]] ...
               & [(me/not (me/or [:leading-space & _]
                                 [:bullet & _]))
                  & _]])
    [(str/join "" !txt) !txt]
    _ "no")


  (let [ed-value (.-children @ted)]
    (->> (macro-parse-lines {:children ed-value})
         (group-by (comp first :el-ids))
         (#(-> %
               (update-vals (fn [parses]
                              {:macro-parse (first parses)}))))
         )
    )
  )

(defn editor [{:keys [text on-evt ed-atom]}]
  (rg/with-let [initial-value (text->pre-norm-ev text)
                ed-value (rg/atom initial-value)
                key-chord (rg/atom [])
                parsed (rg/atom {})
                _ @(rg/track!
                        (fn []
                          (->> (macro-parse-lines {:children @ed-value})
                               ;; (group-by (comp first :el-ids))
                               (into {}
                                 (map (fn [x]
                                        [(-> x :el-ids first) x])))
                               ;; (#(-> % (update-vals first)))
                               ;; (#(doto % prn))
                               (swap! parsed assoc :macro-parse)
                               #_
                               (swap! parsed (fn swap-old-new [old new]
                                               ;; removes unparsed things
                                               ;; and only takes new parse
                                               (into {}
                                                 (map (fn [[k v]]
                                                        (deep-merge (-> (get old k)
                                                                        (dissoc :macro-parse))
                                                                    new)))
                                                 new))))))
                ]
    (let [[ed] (react/useState (fn [] (-> (slate/createEditor)
                                          slate-react/withReact
                                          with-rich-org
                                          ;; with-break-indent
                                          )))]

      (when ed-atom
        (reset! ed-atom ed))
      ;; (reset! ted ed)
      ;; TODO
      ;; - set up links
      ;; - create linkable node
      ;; - macro parse
      ;;   - annotate?
      ;;   - problem is how do you tie back to structure.
      ;;   - probably better to do a one-off. no benefit to shared dependency tbh
      ;; on change modify the macro parse
      ;; use it for display in the render element. boom.
      ;;
      ;; ok so key chord node operations are fine, but autocomplete,
      ;; maybe should be handled by a with-ed wrapper thing
      ;; - just feels weird to have line breaks handled in one place, but like other
      ;;  autocomplete stuff handled elsewhere?
      ;; - thing is the line break thing (probably others) depend on
      ;;   - pre-update-change state
      ;;   - (possibly) post update changes
      [:> slate-react/Slate {:editor       ed
                             :initialValue initial-value
                             :onChange     (fn [value]
                                             (let [sel (ed-selection ed)]
                                               (if (js/ed.operations.some
                                                    (fn [op] (= "set_selection"
                                                                js/op.type)))
                                                 (do
                                                   (reset! key-chord [])
                                                   (on-evt {:ed-evt       :selection
                                                            ;;:ed-el  (ed-node+path ed (:anchor sel))
                                                            :ed-selection sel}))
                                                 (do
                                                   ;; (klog "ops" js/ed.operations)
                                                   (reset! ed-value value)
                                                   ;; (ed-parse-paths ed)
                                                   (on-evt {:ed-evt       :change
                                                            :ed-selection sel
                                                            :ed-text      (ed->text {:children value})})))))}
       [:> slate-react/Editable
        {:style         {:padding   ".25em"
                         :margin    "-.25em"
                         :z-index   0
                         :width "100%"
                         :min-width "0.25em"
                         :outline   "none"
                         :display   "inline-block"}
         :onKeyDown
         (fn ed-keydown [e]
           (let [k (.-key e)]
             (when-not (#{"Shift"} k)
               (let [max-keys 2
                     chord
                     (swap! key-chord (fn [sv]
                                        (-> sv
                                            (conj k)
                                            (subvec (max 0 (- (inc (count sv))
                                                              max-keys))))
                                        ))]
                 (ed-handle-key-chord ed chord e)
                 ;; (when (count chord))
                 (on-evt {:ed-evt :word
                          :js-evt e
                          :ed-key-chord chord})))))
         :renderElement (fn render-edit-el [props]
                          (let [{:keys [el-id] :as el} (js->clj js/props.element :keywordize-keys true)]
                            (rg/as-element
                             [slate-element {:slate-props props
                                             :parsed      parsed
                                             :on-evt      on-evt}])))}]])))
