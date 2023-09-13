(ns transpile.sql-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [transpile.dialect :as dialect]
   [transpile.test-util :as test-util]))

(deftest field-test
  (is (= "\"name\""
         (dialect/clause->sql :sql test-util/fields [:field 2]))))

(deftest is-empty-test
  (is (= "NULL IS NULL"
         (dialect/clause->sql :sql nil [:is-empty nil])))
  (is (= "\"name\" IS NULL"
         (dialect/clause->sql :sql test-util/fields [:is-empty [:field 2]])))
  (is (= "10 IS NULL"
         (dialect/clause->sql :sql test-util/fields [:is-empty 10])))
  (is (= "'hello' IS NULL"
         (dialect/clause->sql :sql test-util/fields [:is-empty "hello"]))))

(deftest not-empty-test
  (is (= "NULL IS NOT NULL"
         (dialect/clause->sql :sql nil [:not-empty nil])))
  (is (= "\"name\" IS NOT NULL"
         (dialect/clause->sql :sql test-util/fields [:not-empty [:field 2]])))
  (is (= "10 IS NOT NULL"
         (dialect/clause->sql :sql test-util/fields [:not-empty 10])))
  (is (= "'hello' IS NOT NULL"
         (dialect/clause->sql :sql test-util/fields [:not-empty "hello"]))))

(deftest <-test
  (is (= "\"id\" < \"age\""
         (dialect/clause->sql :sql test-util/fields [:< [:field 1] [:field 4]])))
  (is (= "10 < \"age\""
         (dialect/clause->sql :sql test-util/fields [:< 10 [:field 4]])))
  (is (= "10 < 20"
         (dialect/clause->sql :sql test-util/fields [:< 10 20]))))

(deftest >-test
  (is (= "\"id\" > \"age\""
         (dialect/clause->sql :sql test-util/fields [:> [:field 1] [:field 4]])))
  (is (= "10 > \"age\""
         (dialect/clause->sql :sql test-util/fields [:> 10 [:field 4]])))
  (is (= "10 > 20"
         (dialect/clause->sql :sql test-util/fields [:> 10 20]))))

(deftest =-test
  (testing "= clause with nil arg"
    (is (= "NULL IS NULL"
           (dialect/clause->sql :sql test-util/fields [:= nil nil])))
    (is (= "20 IS NULL"
           (dialect/clause->sql :sql test-util/fields [:= nil 20])))
    (is (= "\"age\" IS NULL"
           (dialect/clause->sql :sql test-util/fields [:= [:field 4] nil])))
    (is (= "'asdf' IS NULL"
           (dialect/clause->sql :sql test-util/fields [:= nil "asdf"]))))
  (testing "= clause with 2 args"
    (is (= "\"age\" = 20"
           (dialect/clause->sql :sql test-util/fields [:= [:field 4] 20])))
    (is (= "\"age\" = \"id\""
           (dialect/clause->sql :sql test-util/fields [:= [:field 4] [:field 1]])))
    (is (= "10 = 20"
           (dialect/clause->sql :sql test-util/fields [:= 10 20])))
    (is (= "10 = 'asdf'"
           (dialect/clause->sql :sql test-util/fields [:= 10 "asdf"])))
    (is (= "'x' = 'asdf'"
           (dialect/clause->sql :sql test-util/fields [:= "x" "asdf"]))))
  (testing "= clause with multiple args"
    (is (= "\"age\" IN (20, 'asdf', \"id\", NULL)"
           (dialect/clause->sql :sql test-util/fields [:= [:field 4] 20 "asdf" [:field 1] nil])))))

(deftest !=-test
  (testing "!= clause with nil arg"
    (is (= "NULL IS NOT NULL"
           (dialect/clause->sql :sql test-util/fields [:!= nil nil])))
    (is (= "20 IS NOT NULL"
           (dialect/clause->sql :sql test-util/fields [:!= nil 20])))
    (is (= "\"age\" IS NOT NULL"
           (dialect/clause->sql :sql test-util/fields [:!= [:field 4] nil])))
    (is (= "'asdf' IS NOT NULL"
           (dialect/clause->sql :sql test-util/fields [:!= nil "asdf"]))))
  (testing "= clause with 2 args"
    (is (= "\"age\" <> 20"
           (dialect/clause->sql :sql test-util/fields [:!= [:field 4] 20])))
    (is (= "\"age\" <> \"id\""
           (dialect/clause->sql :sql test-util/fields [:!= [:field 4] [:field 1]])))
    (is (= "10 <> 20"
           (dialect/clause->sql :sql test-util/fields [:!= 10 20])))
    (is (= "10 <> 'asdf'"
           (dialect/clause->sql :sql test-util/fields [:!= 10 "asdf"])))
    (is (= "'x' <> 'asdf'"
           (dialect/clause->sql :sql test-util/fields [:!= "x" "asdf"]))))
  (testing "= clause with multiple args"
    (is (= "\"age\" NOT IN (20, 'asdf', \"id\", NULL)"
           (dialect/clause->sql :sql test-util/fields [:!= [:field 4] 20 "asdf" [:field 1] nil])))))


