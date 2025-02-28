(ns org-browser-editor.index
  (:require
   [reagent.core :as rg]
   ["react-dom/client" :as react-client]
   [org-browser-editor.slate-rich-org :as rich-org]
   ))

(defn main-page []
  [:<>
   [:p "hello2!"]
   [:div {:style {:border "2px solid black"}}
    [rich-org/editor {:text "this is a test"
                      :on-evt (fn [x]
                               ; (js/console.log "ed event")
                                )}]]])

(defonce rroot (react-client/createRoot (js/document.querySelector "#app")))

(rg/set-default-compiler! (rg/create-compiler {:function-components true}))

(defn ^:dev/after-load render []
  (.render
   rroot
   (rg/as-element [main-page])))

(defn ^:export init []
  (render))
