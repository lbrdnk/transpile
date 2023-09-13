(ns transpile.sql-test
  (:require
   [clojure.test :refer [deftest is]]
   [transpile.dialect :as dialect]
   [transpile.test-util :as test-util]))

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


