(ns org-browser-editor.util)

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
