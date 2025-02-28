(ns org-browser-editor.util
  (:require
   ["slate" :as slate])
  (:require-macros [org-browser-editor.util :refer [ed-wo-norm]]))

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

(defn ->uuid []
  (str (random-uuid)))
