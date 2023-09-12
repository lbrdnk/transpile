(ns transpile.transpile-test
  (:require [clojure.test :refer [is testing deftest run-test]]
            [transpile.transpile :refer [generate-sql where->sql]]))

(def fields {1 :id
             2 :name
             3 :date_joined
             4 :age})

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
