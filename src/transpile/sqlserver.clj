(ns transpile.sqlserver
  (:require
   [clojure.string :as str]
   [transpile.dialect :as dialect]
   [transpile.util :as util]))

(dialect/derive! :sqlserver :sql)

(defmethod dialect/generate-sql :sqlserver
  [_dialect fields {:keys [limit where] :as _query}]
  (let [limit-sql (when (some? limit) (str "TOP " limit))
        base-sql (str/join " " (filter some? ["SELECT" limit-sql "* FROM data"]))
        where-sql (when (some? where) (str "WHERE " (dialect/clause->sql :sqlserver fields where)))]
    (util/sql-parts->sql-query base-sql where-sql)))