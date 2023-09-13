(ns transpile.util-test
  (:require
   [clojure.test :refer [is deftest]]
   [transpile.core :refer [generate-sql]]
   [transpile.util :as util]
   [transpile.test-util :as test-util]))

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

(deftest expand-macros-test
  (is (= [:> [:field 4] 18]
         (util/expand-macros [:macro "is_adult"] test-util/macros)))
  (is (= [:= [:field 2] "joe"]
       (util/expand-macros [:macro "is_joe"] test-util/macros)))
  (is (= [:and [:= [:field 2] "joe"] [:> [:field 4] 18]]
         (util/expand-macros [:macro "is_adult_joe"] test-util/macros)))
  (is (= "SELECT * FROM data WHERE \"id\" <= 3 OR \"name\" = 'joe' AND \"age\" > 18;"
         (generate-sql :postgres test-util/fields {:where [:or
                                                           [:not [:> [:field 1] 3]]
                                                           [:macro "is_adult_joe"]]} test-util/macros)))
  (is (thrown? Exception
               (generate-sql :postgres
                             test-util/fields
                             {:where [:or [:not [:> [:field 1] 3]] [:macro "is_good"]]}
                             test-util/circular-macros))))

(deftest simplify-clause-test
  (is (= [:and [:> 3 2] [:= "a" "b"] [:is-empty [:field 2]]]
         (util/simplify-clause [:and [:and [:> 3 2] [:= "a" "b"]] [:is-empty [:field 2]]])))
  (is (= [:or [:> 3 2] [:= "a" "b"] [:is-empty [:field 2]]]
         (util/simplify-clause [:or [:or [:> 3 2] [:= "a" "b"]] [:is-empty [:field 2]]])))
  (is (= [:> 3 2]
         (util/simplify-clause [:not [:not [:> 3 2]]]))))