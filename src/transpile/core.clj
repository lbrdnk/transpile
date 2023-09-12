(ns transpile.core
  (:require
   [transpile.dialect :as dialect]
   [transpile.sql]
   [transpile.postgres]
   [transpile.mysql]
   [transpile.sqlserver]))

(defn generate-sql
  [dialect fields query]
  (dialect/generate-sql dialect fields query))