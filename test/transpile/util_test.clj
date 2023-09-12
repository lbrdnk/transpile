(ns transpile.util-test
  (:require
   [clojure.test :refer [is deftest]]
   [transpile.util :as util]))

(deftest negate-test
  (is (= [:is-empty [:field 3]]
         (util/negate [:not-empty [:field 3]])))
  (is (= [:not-empty [:field 3]]
         (util/negate [:is-empty [:field 3]])))
  (is (= [:and [:is-empty [:field 3]] [:not-empty [:field 2]]]
         (util/negate [:or [:not-empty [:field 3]] [:is-empty [:field 2]]])))
  (is (= [:or [:not-empty [:field 3]] [:is-empty [:field 2]]]
         (util/negate [:and [:is-empty [:field 3]] [:not-empty [:field 2]]])))
  (is (= [:= [:field 3] "xixi"]
         (util/negate [:!= [:field 3] "xixi"])))
  (is (= [:!= [:field 3] 1 2 3]
         (util/negate [:= [:field 3] 1 2 3])))
  (is (= [:<= [:field 3] 1]
         (util/negate [:> [:field 3] 1])))
  (is (= [:>= [:field 3] 1]
         (util/negate [:< [:field 3] 1])))
  (is (= [:= [:field 3] 1]
         (util/negate [:not [:= [:field 3] 1]]))))

(comment
  (clojure.test/run-test negate-test)
  )