((nil . ((eval . (require 'cider))
         (cider-jack-in-cmd . "clojure -M:local-dev:dev:cljs")
         (cider-custom-cljs-repl-init-form . "
(do
  (require '[shadow.cljs.devtools.api :as shadow.api]
           '[shadow.cljs.devtools.server :as shadow.server])
  (def build :app)
  (shadow.server/start!)
  (shadow.api/watch build)
  (shadow.api/nrepl-select build)
)
")
         (cider-default-cljs-repl . custom)
         )))
