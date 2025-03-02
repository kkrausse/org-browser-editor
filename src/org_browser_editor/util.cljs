(ns org-browser-editor.util
  (:require
   ["slate" :as slate]
   [promesa.core :as p])
  (:require-macros [org-browser-editor.util :refer [ed-wo-norm]]))

(def ^:dynamic *debug* {})

(defn ptime [p]
  (p/let [st (system-time)
          r p]
    (println (str "Elapsed time: "
                  (.toFixed (- (system-time) st) 6)
                  "msecs"))
    r))

(defn gen->seq [gen]
    (lazy-seq
     (let [result (.next gen)]  ;; Get the next value from the generator
       (if (.-done result)
         nil                    ;; If the generator is done, return nil (end of seq)
         (cons (.-value result) (gen->seq gen))))))

(defn deep-merge [v & vs]
  (letfn [(rec-merge [v1 v2]
            (cond
              (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              (nil? v2) v1
              :else v2))]
    (reduce #(rec-merge %1 %2) v vs)))

(defn jsonify [obj]
  (-> obj (clj->js) (js/JSON.stringify)))

(defn json-parse [s]
  (-> s (js/JSON.parse) (js->clj :keywordize-keys true)))

(defn ->uuid []
  (str (random-uuid)))

(defn re-quote [s]
  (let [special (set ".?*+^$[]\\(){}|")
        escfn #(if (special %) (str \\ %) %)]
    (apply str (map escfn s))))
