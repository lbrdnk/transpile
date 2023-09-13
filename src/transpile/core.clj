(ns transpile.core
  (:require
   [transpile.dialect :as dialect]
   [transpile.sql]
   [transpile.postgres]
   [transpile.mysql]
   [transpile.sqlserver]
   [transpile.util :as util]))

(defn generate-sql
  ([dialect fields query]
   (dialect/generate-sql dialect fields query))
  ([dialect fields query macros]
   (dialect/generate-sql dialect fields
                         (update query :where #(util/expand-macros % macros)))))