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
   (generate-sql dialect fields query nil))
  ([dialect fields query macros]
   (dialect/generate-sql dialect fields
                         (as-> query $
                             (if (some? macros)
                               (update $ :where #(util/expand-macros % macros))
                               $)
                             (update $ :where util/simplify-clause)))))