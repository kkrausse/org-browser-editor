(ns org-browser-editor.util
  (:require
   [clojure.java.io :as io]
   [promesa.core :as p]))

(defmacro ed-wo-norm
  {:style/indent :defn}
  [ed & forms]
  `(slate/Editor.withoutNormalizing
    ~ed
    (fn ~'wo-norm-fn []
      ~@forms)))

(defmacro add-classes
  {:style/indent :defn}
  [graph & kvs]
  `(me/rewrite ~graph
    ~@(->> kvs
         (partition 2)
         (mapcat
          (fn [[pat mk-class]]
            `[(me/and [!total# ...]
                      ~pat)
             [!total# {:classes ~mk-class} . !total# ...]]))
         )
    [!x# ...] [(me/cata !x#) ...]
    ?x# ?x#))

(defmacro slurpped-str [f]
  (slurp f))

(defmacro klog [msg & vars]
  `(cljs.pprint/pprint
    ~(->> vars
          (map (fn [v]
                 [(cond
                    (symbol? v) (keyword v)
                    (string? v) (keyword (gensym "str"))
                    :else (keyword (gensym "expr")))
                  `(cljs.core/js->clj ~v :keywordize-keys true)]
                 ))
          (into {:msg msg
                 :debug `*debug*}))))
