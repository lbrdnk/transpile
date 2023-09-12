(ns transpile.transpile-test
  (:require
   [clojure.test :refer [is testing deftest run-test]]
   [transpile.core :refer [generate-sql]]
   [transpile.util :as util]))

(def fields {1 :id
             2 :name
             3 :date_joined
             4 :age})

;;; TODO later enhance to mysql, make separate namespaces for that
(deftest provided-postgres-test
  (testing "Test examples from assignment"
    (is (= "SELECT * FROM data WHERE \"date_joined\" IS NULL;"
           (generate-sql :postgres fields {:where [:= [:field 3] nil]})))
    (is (= "SELECT * FROM data WHERE \"age\" > 35;"
           (generate-sql :postgres fields {:where [:> [:field 4] 35]})))

    (is (= "SELECT * FROM data WHERE \"id\" < 5 AND \"name\" = 'joe';"
           (generate-sql :postgres fields {:where [:and [:< [:field 1] 5] [:= [:field 2] "joe"]]})))

    (is (= "SELECT * FROM data WHERE \"date_joined\" <> '2015-11-01' OR \"id\" = 456;"
           (generate-sql :postgres fields {:where [:or [:!= [:field 3] "2015-11-01"] [:= [:field 1] 456]]})))

    (is (= "SELECT * FROM data WHERE \"date_joined\" IS NOT NULL AND (\"age\" > 25 OR \"name\" = 'Jerry');"
           (generate-sql :postgres fields {:where [:and
                                                   [:!= [:field 3] nil]
                                                   ;; modified ">" to :>
                                                   [:or [:> [:field 4] 25] [:= [:field 2] "Jerry"]]]})))

    (is (= "SELECT * FROM data WHERE \"age\" IN (25, 26, 27);"
           (generate-sql :postgres fields {:where [:= [:field 4] 25 26 27]})))

    (is (= "SELECT * FROM data WHERE \"name\" = 'cam';"
           (generate-sql :postgres fields {:where [:= [:field 2] "cam"]})))

    (is (= "SELECT * FROM data LIMIT 20;"
           (generate-sql :postgres fields {:limit 20})))))

(comment
  (run-test provided-postgres-test)
  )

(deftest transpile-nil-test
  (is (= "SELECT * FROM data WHERE \"date_joined\" IN (10, NULL);"
         (generate-sql :postgres fields {:where [:= [:field 3] 10 nil]}))))

(comment
  (run-test transpile-nil-test)
  )

#_(is (= "SELECT * FROM data WHERE name = 'cam' LIMIT 10;"
       (generate-sql :mysql fields {:where [:= [:field 2] "cam"], :limit 10})))
#_(generate-sql :sqlserver fields {:limit 20})
;; -> "SELECT TOP 20 * FROM data;"


(deftest sql-server-limit-test
  (testing "Sqlserver limit"
    (is (= "SELECT TOP 20 * FROM data;"
           (generate-sql :sqlserver fields {:limit 20}))))
  (testing "Postgres limit"
    (is (= "SELECT * FROM data LIMIT 20;"
           (generate-sql :postgres fields {:limit 20})))))


;;;; TODO proper quoting
(deftest is-null-=-test
  (is (= "SELECT * FROM data WHERE \"date_joined\" IS NULL;"
       (generate-sql :postgres fields {:where [:= [:field 3] nil]})))
  #_(is (= "SELECT * FROM data WHERE \"date_joined\" = \"2015-11-01\";"
         (generate-sql :postgres fields {:where [:= [:field 3] nil]})))
  (is (= "SELECT * FROM data WHERE \"name\" = 'joe';"
         (generate-sql :postgres fields {:where [:= [:field 2] "joe"]})))
  (is (= "SELECT * FROM data WHERE \"age\" IN (25, 26, 27);"
         (generate-sql :postgres fields {:where [:= [:field 4] 25 26 27]}))))

(comment
  (run-test is-null-=-test))

;; okish
(deftest lt-gt-test
  (is (= "SELECT * FROM data WHERE \"age\" > 35;"
         (generate-sql :postgres fields {:where [:> [:field 4] 35]}))))

(comment
  (run-test lt-gt-test)
  )

;; okish
(deftest and-test
  (is (= "SELECT * FROM data WHERE \"id\" < 5 AND \"name\" = 'joe';"
         (generate-sql :postgres fields {:where [:and [:< [:field 1] 5] [:= [:field 2] "joe"]]}))))

(comment
  (run-test and-test)
  )

(deftest or-test
  (is (= "SELECT * FROM data WHERE \"date_joined\" <> '2015-11-01' OR \"id\" = 456;"
         (generate-sql :postgres fields {:where [:or [:!= [:field 3] "2015-11-01"] [:= [:field 1] 456]]}))))

(comment
  (run-test or-test)
  )


(comment
  (run-test sql-server-limit-test)
  (generate-sql :postgres fields {:where [:= [:field 3] nil]})
  ;; -> "SELECT * FROM data WHERE date_joined IS NULL;"
  
  (generate-sql :postgres fields {:where [:> [:field 4] 35]})
  ;; -> "SELECT * FROM data WHERE age > 35;"
  
  (generate-sql :postgres fields {:where [:and [:< [:field 1] 5] [:= [:field 2] "joe"]]})
  ;; -> "SELECT * FROM data WHERE id < 5 AND name = 'joe';"
  
  (generate-sql :postgres fields {:where [:or [:!= [:field 3] "2015-11-01"] [:= [:field 1] 456]]})
  ;; -> "SELECT * FROM data WHERE date_joined <> '2015-11-01' OR id = 456;"
  
  (generate-sql :postgres fields {:where [:and [:!= [:field 3] nil] [:or [">" [:field 4] 25] [:= [:field 2] "Jerry"]]]})
  ;; -> "SELECT * FROM data WHERE date_joined IS NOT NULL AND (age > 25 OR name = 'Jerry');"
  
  (generate-sql :postgres fields {:where [:= [:field 4] 25 26 27]})
  ;; -> "SELECT * FROM data WHERE age IN (25, 26, 27);"
  
  (generate-sql :postgres fields {:where [:= [:field 2] "cam"]})
  ;; -> "SELECT * FROM data WHERE name = 'cam';"
  
  (generate-sql :mysql fields {:where [:= [:field 2] "cam"], :limit 10})
  ;; -> "SELECT * FROM data WHERE name = 'cam' LIMIT 10;"
  
  (generate-sql :postgres fields {:limit 20})
  ;; -> "SELECT * FROM data LIMIT 20;"
  
  (generate-sql :sqlserver fields {:limit 20})
  ;; -> "SELECT TOP 20 * FROM data;"
  )



#_(deftest is-null-=-test
    (is (= "age"
           (where->sql :postgres fields [:field 3]))))



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
  (run-test negate-test)
  )