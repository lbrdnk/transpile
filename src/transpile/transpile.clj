(ns transpile.transpile
  (:require [clojure.string :as str]))

(comment
  (def fields {1 :id
               2 :name
               3 :date_joined
               4 :age})
  (where->sql :postgres nil [:= [:field 3] nil])
  )


(declare where->sql)

(declare add-closing-semicolon)


(defmulti generate-sql 
  ""
  (fn [dialect fields query] dialect))

;;;; TODO maybe assertions for limits
(defmethod generate-sql :postgres
  [dialect fields {:keys [limit where] :as query}]
  (let [base-sql "SELECT * FROM data"
        where-sql (when (vector? where) (str "WHERE " (where->sql dialect fields where)))
        limit-sql (when (some? limit) (str "LIMIT " limit))]
    (-> (str/join " " (keep not-empty [base-sql where-sql limit-sql]))
        add-closing-semicolon)))

#_(defmethod generate-sql :mysql
  [_dialect fields query]
  (let [base-query "SELECT * FROM \"data\""]
    (-> handle-where
        handle-limit)))

(defn add-closing-semicolon [sql] (str sql ";"))

(defmethod generate-sql :sqlserver
  [_dialect fields {:keys [limit where] :as _query}]
  (let [limit-sql (when (some? limit) (str "TOP " limit))
        base-sql (str/join " " (filter not-empty ["SELECT" limit-sql "* FROM data"]))
        #_#_where-sql (where->sql :sqlserver fields where)]
    (-> base-sql add-closing-semicolon)))

(comment
  (generate-sql :postgres {} {})
  (generate-sql :sqlserver {} {:limit 10})
  )

;;;; where clause

(defn- dispatch-where->sql [dialect fields clause]
  (def x clause)
  (if (vector? clause)
    [dialect (first clause)]
    [dialect (type clause)]))

;; i dont think we need query as an arg
(def where->sql nil)
(defmulti where->sql #'dispatch-where->sql)

;; todo make it muulti, this is just temporary
(defn quote-name [dialect field-name]
  (case dialect
    :postgres  (str "\"" field-name "\"")
    :mysql     (str "`" field-name "`")
    :sqlserver (str "\"" field-name "\"")))


;; (generate-sql :postgres fields {:where [:= [:field 3] nil]})
;; TODO make 'default' aka sql to 'dry'
(defmethod where->sql [:postgres :field]
  [dialect fields [_field id :as _clause]]
  (assert (contains? fields id) "Mapping for id not found.")
  (quote-name dialect (name (fields id))))

(comment
  (where->sql :postgres fields [:field 3])
  )

(defn non-nil-arg [[a1 a2]]
  (if (nil? a1) a2 a1))

(defmethod where->sql [:postgres :is-empty]
  [dialect fields [_type arg]]
  (str (where->sql dialect fields arg) " IS NULL"))

(defmethod where->sql [:postgres :not-empty]
  [dialect fields [_type arg]]
  (str (where->sql dialect fields arg) " IS NOT NULL"))

(comment
  (where->sql :postgres fields [:is-empty [:field 3]])
  )

;;;; This looks ok-ish
;;;; TODO: more tests.
(defmethod where->sql [:postgres :=]
  [dialect fields clause]
  (let [args (subvec clause 1)]
    (cond (and (= (count args) 2)
               (some nil? args))
          (where->sql dialect fields [:is-empty (non-nil-arg args)])

          (= (count args) 2)
          (let [first-arg-sql (where->sql dialect fields (args 0))
                second-arg-sql (where->sql dialect fields (args 1))]
            (str first-arg-sql " = " second-arg-sql))

          :else
          (let [first-arg-sql (where->sql dialect fields (args 0))
                rest-args-sql (mapv (partial where->sql dialect fields) (subvec args 1))]
            (str first-arg-sql " IN (" (str/join ", " rest-args-sql) ")")))))

(comment
  (where->sql :postgres fields [:= [:field 3] nil])
  (where->sql :postgres fields [:= [:field 3] 5 6 7]))

;; TODO dry it!
(defmethod where->sql [:postgres :!=]
  [dialect fields clause]
  (let [args (subvec clause 1)]
    (cond (and (= (count args) 2)
               (some nil? args))
          (where->sql dialect fields [:not-empty (non-nil-arg args)])

          ;; TODO into two-comparator
          (= (count args) 2)
          (let [first-arg-sql (where->sql dialect fields (args 0))
                second-arg-sql (where->sql dialect fields (args 1))]
            (str first-arg-sql " <> " second-arg-sql))

          :else
          (let [first-arg-sql (where->sql dialect fields (args 0))
                rest-args-sql (mapv (partial where->sql dialect fields) (subvec args 1))]
            (str first-arg-sql " NOT IN (" (str/join ", " rest-args-sql) ")")))))

(comment
  (where->sql :postgres fields [:!= nil [:field 3]])
  (where->sql :postgres fields [:!= "joe" [:field 3]])
  (where->sql :postgres fields [:!= [:field 2] "john" "doe"])
  )

(defmethod where->sql [:postgres String]
  [dialect fields clause]
  (str "'" clause "'"))

;; base, currently just for numbers, TODO, think it through, maybe i should enumerate all
(defmethod where->sql [:postgres Object]
  [dialect fields clause]
  (str clause))

(comment
  (where->sql :postgres fields "ahoj")
  )

(defmethod where->sql [:postgres :>]
  [dialect fields [_ arg-1 arg-2]]
  (str (where->sql dialect fields arg-1) " > " (where->sql dialect fields arg-2)))

(defmethod where->sql [:postgres :<]
  [dialect fields [_ arg-1 arg-2]]
  (str (where->sql dialect fields arg-1) " < " (where->sql dialect fields arg-2)))

(comment
  (where->sql :postgres fields [:> [:field 4] 35])
  )

(defmethod where->sql [:postgres :and]
  [dialect fields clause]
  (str/join " AND "
            (->> (subvec clause 1)
                 (map (partial where->sql dialect fields))
                 ;; TODO: Following, maybe redundant. Examine later.
                 (filter not-empty))))

(defmethod where->sql [:postgres :or]
  [dialect fields clause]
  (str/join " OR "
            (->> (subvec clause 1)
                 (map (partial where->sql dialect fields))
                 ;; TODO: Following, maybe redundant. Examine later.
                 (filter not-empty))))
