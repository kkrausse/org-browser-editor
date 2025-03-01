(ns org-browser-editor.roam.searchbox
  (:require
   ["react" :as react]
   [clojure.string :as str]
   [reagent.core :as rg]
   [org-browser-editor.util :refer [re-quote klog]]))

(defn searchbox
  "candidates are maps with :text key which will be displayed & whatever else
  this map is returned in on-select"
  [& {:keys [on-blur on-select candidates focus? max-candidates]
      :or {max-candidates 30}}]
  (rg/with-let [search (rg/atom "")
                selected (rg/atom 0)
                display? (rg/atom true)]
    (let [input-ref     (react/useRef)
          container-ref (react/useRef)
          on-select (fn [x]
                      (on-select x)
                      (reset! display? false))
          ranked-candidates
          (rg/track
           (fn []
             (->> candidates
                  ;; NOTE: need like gather parent info too
                  ;;
                  ;; get matches & match score
                  (map (fn [{:keys [text] :as cand}]
                         (let [text (str/lower-case text)
                               xs (str/split (str/lower-case @search) #" ")
                               matches
                               (->> xs
                                    (reductions
                                     (fn [{:keys [match-num current-text]} x]
                                       #_
                                       (klog "doing candidate"
                                             text x)
                                       (let [idx     (str/index-of text x)
                                             cur-idx (str/index-of current-text x)]
                                         {:current-text (str/replace-first current-text x "")
                                          :match-info   {:idx   (when cur-idx idx)
                                                         :x     x
                                                         :score (if cur-idx
                                                                  ;; idk weird score
                                                                  ;; weights first stuff better
                                                                  (+ (- 20 (* 1 cur-idx))
                                        ;(- 5 (* 0.5 match-num))
                                                                     (* 10 (count x)))
                                                                  0)}}))
                                     {:current-text text
                                      :match-num 0})
                                    (drop 1)
                                    (map :match-info))]
                           {:candidate cand
                            :matches   matches
                            :score     (->> matches (map :score) (reduce +))})))
                  (sort-by (comp #(* -1 %) :score))
                  (into []))))]
      (react/useEffect (fn []
                         (when focus?
                           (some-> input-ref (.-current) (.focus)))
                         (constantly nil))
                       (clj->js [focus?]))
      [:div {
             ;; :style     {:z-index          5
             ;;             :background-color "black"
             ;;             ;;:border           "2px solid darkgreen"
             ;;             :position         "absolute"}
             :onBlur    (fn [x]
                          (reset! selected 0)
                          (reset! display? false)
                          (on-blur x))
             :ref       container-ref
             :onKeyDown (fn [e]
                          (let [no-default (fn [& args] (.preventDefault e))]
                            (case (.-key e)
                              ("Tab" "ArrowDown") (no-default (swap! selected
                                                                     #(min (inc %) (dec (count @ranked-candidates)))))
                              "ArrowUp"           (swap! selected #(max 0 (dec %)))
                              "Enter"             (on-select (-> @ranked-candidates
                                                                 (get @selected)
                                                                 :candidate))
                              nil)))}
       [:input {:type      "text"
                :ref       input-ref
                :value     @search
                :on-focus (fn [_] (reset! display? true))
                :on-change (fn [e] (reset! search js/e.target.value))}]
       (when @display?
         (->> @ranked-candidates
              (map-indexed (fn [i {{:keys [text]
                                    :as   candidate} :candidate
                                   ms                :matches}]
                             ^{:key i} [:div {:style        {:cursor           "pointer"
                                                             :background-color (when (= @selected i) "darkgreen")
                                                             :border           "1px solid grey"}
                                              :onMouseEnter (fn [e] (reset! selected i))
                                              :onMouseDown (fn [e]
                                                             ;; prevent blur
                                                             (.preventDefault e))
                                              :onClick     (fn [e]
                                                             (on-select candidate))}
                                        (->> ms
                                             (filter :idx)
                                             (sort-by :idx)
                                             (reduce (fn [items {:keys [idx x]}]
                                                       (let [iidx       (dec (count items))
                                                             _ (when-not (get items iidx)
                                                                 (klog "busted" idx x items))
                                                             cs         (str/lower-case (get items iidx))
                                                             [pre post] (str/split cs (re-pattern (re-quote x)) 2)]
                                                         (if (< 0 (count x))
                                                           (-> items
                                                               (assoc iidx pre)
                                                               (conj [:mark x])
                                                               (conj post))
                                                           items)))
                                                     [:<> text]))]))
              (take max-candidates)
              (doall)))])))
