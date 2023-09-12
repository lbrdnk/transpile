(ns transpile.postgres-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [transpile.core :refer [generate-sql]]
   [transpile.test-util :as test-util]))

(deftest provided-test
  (testing "Test examples from assignment"
    (is (= "SELECT * FROM data WHERE \"date_joined\" IS NULL;"
           (generate-sql :postgres test-util/fields {:where [:= [:field 3] nil]})))
    (is (= "SELECT * FROM data WHERE \"age\" > 35;"
           (generate-sql :postgres test-util/fields {:where [:> [:field 4] 35]})))

    (is (= "SELECT * FROM data WHERE \"id\" < 5 AND \"name\" = 'joe';"
           (generate-sql :postgres test-util/fields {:where [:and [:< [:field 1] 5] [:= [:field 2] "joe"]]})))

    (is (= "SELECT * FROM data WHERE \"date_joined\" <> '2015-11-01' OR \"id\" = 456;"
           (generate-sql :postgres test-util/fields {:where [:or [:!= [:field 3] "2015-11-01"] [:= [:field 1] 456]]})))

    (is (= "SELECT * FROM data WHERE \"date_joined\" IS NOT NULL AND (\"age\" > 25 OR \"name\" = 'Jerry');"
           (generate-sql :postgres test-util/fields {:where [:and
                                                              [:!= [:field 3] nil]
                                                              ;; modified ">" to :>
                                                              [:or [:> [:field 4] 25] [:= [:field 2] "Jerry"]]]})))

    (is (= "SELECT * FROM data WHERE \"age\" IN (25, 26, 27);"
           (generate-sql :postgres test-util/fields {:where [:= [:field 4] 25 26 27]})))

    (is (= "SELECT * FROM data WHERE \"name\" = 'cam';"
           (generate-sql :postgres test-util/fields {:where [:= [:field 2] "cam"]})))))

(comment
  
  (clojure.test/run-test provided-test)
  )