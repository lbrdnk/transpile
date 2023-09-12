(ns transpile.postgres
  (:require [transpile.dialect :as dialect]))

(dialect/derive! :postgres :sql)
