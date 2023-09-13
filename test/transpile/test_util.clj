(ns transpile.test-util)

(def fields {1 :id
             2 :name
             3 :date_joined
             4 :age})

(def macros {"is_joe"       [:= [:field 2] "joe"]
             ;; changed ">" to :>
             "is_adult"     [:> [:field 4] 18]
             "is_adult_joe" [:and [:macro "is_joe"] [:macro "is_adult"]]})