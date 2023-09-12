(ns transpile.dialect)

(defonce hierarchy (make-hierarchy))

(defn derive! [dialect parent]
  (alter-var-root #'hierarchy (fn [h] (derive h dialect parent))))

(defmulti quote-field-name
  (fn [dialect _field-name] dialect)
  :hierarchy #'hierarchy)

(defn- dispatch-clause->sql [dialect _fields clause]
  (if (vector? clause)
    [dialect (first clause)]
    [dialect (type clause)]))

(defmulti clause->sql
  dispatch-clause->sql
  :hierarchy #'hierarchy)


(defn- dispatch-generate-sql [dialect _fields _query] dialect)

(defmulti generate-sql
  "Transform x to sql"
  dispatch-generate-sql
  :hierarchy #'hierarchy)

