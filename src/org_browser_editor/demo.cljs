(ns org-browser-editor.demo
  (:require
   [reagent.core :as rg]
   ["react-dom/client" :as react-client]
   [org-browser-editor.slate-rich-org :as rich-org]
   [org-browser-editor.util :as util]
   ))

(defn main-page []
  [:<>
   [:div {:style {:max-width "80ch"}}
    [rich-org/editor {:text
                      (util/slurp-resource "demo.org")
                      :on-evt (fn [x]
                               ;; nothing
                                )}]]])

(defonce rroot (react-client/createRoot (js/document.querySelector "#app")))

(rg/set-default-compiler! (rg/create-compiler {:function-components true}))

(defn ^:dev/after-load render []
  (.render
   rroot
   (rg/as-element [main-page])))

(defn ^:export init []
  (render))
