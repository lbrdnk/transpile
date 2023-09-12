(ns transpile.sql
  (:require
   [clojure.string :as str]
   [transpile.dialect :as dialect]
   [transpile.util :as util]))

;; this is not necessary
#_(dialect/derive! :sql nil)

;;;; [[quote-field-name]]

(defmethod dialect/quote-field-name :sql
  [_dialect field-name]
  (str "\"" field-name "\""))

;;;; [[clause->sql]]

#_(defmethod clause->sql
  [_dialect fields clause])

(defmethod dialect/clause->sql [:sql nil]
  [_dialect _fields clause]
  "NULL")

(defmethod dialect/clause->sql [:sql String]
  [_dialect _fields clause]
  (str "'" clause "'"))

;;;; TODO check that this is ok! used for numbers and null
(defmethod dialect/clause->sql [:sql Object]
  [_dialect _fields clause]
  clause)



(defmethod dialect/clause->sql [:sql :not]
  [dialect fields [_ inner-clause]]
  (dialect/clause->sql dialect fields (util/negate inner-clause)))

(defmethod dialect/clause->sql [:sql :field]
  [dialect fields [_field id :as _clause]]
  (assert (contains? fields id) "Mapping for id not found.")
  (dialect/quote-field-name dialect (name (fields id))))

(comment
  (def fields {1 :id
               2 :name
               3 :date_joined
               4 :age})
  (dialect/clause->sql :postgres fields [:field 3])
  (dialect/clause->sql :sqlserver fields [:field 3])
  (dialect/clause->sql :mysql fields [:field 3])
  )

(defmethod dialect/clause->sql [:sql :is-empty]
  [dialect fields [_type arg]]
  (str (dialect/clause->sql dialect fields arg) " IS NULL"))

(defmethod dialect/clause->sql [:sql :not-empty]
  [dialect fields [_type arg]]
  (str (dialect/clause->sql dialect fields arg) " IS NOT NULL"))

(comment
  (dialect/clause->sql :postgres transpile.test-util/fields [:is-empty [:field 3]])
  (dialect/clause->sql :sqlserver transpile.test-util/fields [:is-empty [:field 3]])
  (dialect/clause->sql :mysql transpile.test-util/fields [:is-empty [:field 3]])

  (dialect/clause->sql :postgres fields [:not-empty [:field 3]])
  (dialect/clause->sql :sqlserver fields [:not-empty [:field 3]])
  (dialect/clause->sql :mysql fields [:not-empty [:field 3]])
  )

(defn- compile-and-join
  "Compile two args and join together with `s`"
  [dialect fields args connector]
  (assert (= 2 (count args)))
  (str (dialect/clause->sql dialect fields (first args))
       " " connector " "
       (dialect/clause->sql dialect fields (second args))))

(defn- ->in-sql
  [dialect fields [first-arg & rest-args] connector]
  (let [first-arg-sql (dialect/clause->sql dialect fields first-arg)
        rest-args-sql (mapv (partial dialect/clause->sql dialect fields) rest-args)]
    (str first-arg-sql " " connector " (" (str/join ", " rest-args-sql) ")")))

(defmethod dialect/clause->sql [:sql :=]
  [dialect fields clause]
  (let [args (subvec clause 1)]
    (cond (and (= (count args) 2)
               (some nil? args))
          (dialect/clause->sql dialect fields [:is-empty (some #(when (some? %) %) args)])

          (= (count args) 2)
          (compile-and-join dialect fields args "=")

          :else
          (->in-sql dialect fields args "IN"))))

(defmethod dialect/clause->sql [:sql :!=]
  [dialect fields clause]
  (let [args (subvec clause 1)]
    (cond (and (= (count args) 2)
               (some nil? args))
          (dialect/clause->sql dialect fields [:not-empty (some #(when (some? %) %) args)])

          (= (count args) 2)
          (compile-and-join dialect fields args "<>")

          :else
          (->in-sql dialect fields args "NOT IN"))))

(comment
  (dialect/clause->sql :mysql fields [:= [:field 3] nil])
  (dialect/clause->sql :mysql fields [:= [:field 3] nil])

  (dialect/clause->sql :mysql fields [:= [:field 3] 1])
  (dialect/clause->sql :mysql fields [:= [:field 3] 1 2])
  (dialect/clause->sql :mysql fields [:= [:field 3] "hi" "how" "are" "you"])

  (dialect/clause->sql :mysql fields [:!= [:field 3] nil])
  (dialect/clause->sql :mysql fields [:!= [:field 3] 1])
  (dialect/clause->sql :mysql fields [:!= [:field 3] 1 2])
  (dialect/clause->sql :mysql fields [:!= [:field 3] "hi" "how" "are" "you"])
  )

(defmethod dialect/clause->sql [:sql :<]
  [dialect fields [_ & args]]
  (assert (= 2 (count args)))
  (compile-and-join dialect fields args "<"))

(defmethod dialect/clause->sql [:sql :<=]
  [dialect fields [_ & args]]
  (assert (= 2 (count args)))
  (compile-and-join dialect fields args "<="))

(defmethod dialect/clause->sql [:sql :>]
  [dialect fields [_ & args]]
  (assert (= 2 (count args)))
  (compile-and-join dialect fields args ">"))

(defmethod dialect/clause->sql [:sql :>=]
  [dialect fields [_ & args]]
  (assert (= 2 (count args)))
  (compile-and-join dialect fields args ">="))

(comment
  (dialect/clause->sql :mysql fields [:> [:field 1] 3])
  (dialect/clause->sql :mysql fields [:< [:field 1] 3])
  (dialect/clause->sql :postgres fields [:< [:field 1] 3])
  )

(defn- transpile-and-maybe-wrap-parens [dialect fields clause]
  (if (and (vector? clause)
           (= (first clause) :or))
    (str "(" (dialect/clause->sql dialect fields clause) ")")
    (dialect/clause->sql dialect fields clause)))

(defmethod dialect/clause->sql [:sql :and]
  [dialect fields clause]
  (str/join " AND " (map (partial transpile-and-maybe-wrap-parens dialect fields) (subvec clause 1))))

(defmethod dialect/clause->sql [:sql :or]
  [dialect fields clause]
  (str/join " OR " (map (partial dialect/clause->sql dialect fields) (subvec clause 1))))

(defmethod dialect/generate-sql :sql
  [dialect fields {:keys [limit where] :as _query}]
  (let [base-sql "SELECT * FROM data"
        where-sql (when (vector? where) (str "WHERE " (dialect/clause->sql dialect fields where)))
        limit-sql (when (some? limit) (str "LIMIT " limit))]
    (util/sql-parts->sql-query base-sql where-sql limit-sql)))