(ns transpile.mysql
  (:require
   [transpile.dialect :as dialect]))

(dialect/derive! :mysql :sql)

(defmethod dialect/quote-field-name :mysql
  [_dialect field-name]
  (str "`" field-name "`"))