(ns transpile.test-util)

(def fields {1 :id
             2 :name
             3 :date_joined
             4 :age})

(def macros {"is_joe"       [:= [:field 2] "joe"]
             ;; changed ">" to :>
             "is_adult"     [:> [:field 4] 18]
             "is_adult_joe" [:and [:macro "is_joe"] [:macro "is_adult"]]})

(def circular-macros {"is_good"   [:and [:macro "is_decent"] [:> [:field 4] 18]]
                      "is_decent" [:and [:macro "is_good"] [:< [:field 5] 5]]})