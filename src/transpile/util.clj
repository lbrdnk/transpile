(ns transpile.util
  (:require
   [clojure.string :as str]
   [clojure.walk :as walk]))

(defn sql-parts->sql-query [& parts]
  (str (str/join " " (filter some? parts)) ";"))

(defmulti negate (fn [[clause & _args]] clause))

(defmethod negate :and
  [[_clause & args]]
  (apply vector :or (map negate args)))

(defmethod negate :or
  [[_clause & args]]
  (apply vector :and (map negate args)))

(defmethod negate :not-empty
  [[_clause & args]]
  (apply vector :is-empty args))

(defmethod negate :is-empty
  [[_clause & args]]
  (apply vector :not-empty args))

(defmethod negate :=
  [[_clause & args]]
  (apply vector :!= args))

(defmethod negate :!=
  [[_clause & args]]
  (apply vector := args))

(defmethod negate :>
  [[_clause & args]]
  (apply vector :<= args))

(defmethod negate :<
  [[_clause & args]]
  (apply vector :>= args))

(defmethod negate :not
  [[_clause inner-clause]]
  inner-clause)

(declare expand-macros)

(def ^:private ^:dynamic *being-expanded* #{})

(defn- expand-macros*
  "Recursively expand macro clause."
  [[_ macro-id] macros]
  (assert (contains? macros macro-id) "Macro is not defined.")
  (when (contains? *being-expanded* macro-id)
    (throw (ex-info "Recursively defined macros not allowed."
                    {:macro-id macro-id
                     :being-expanded *being-expanded*
                     :macros macros})))
  (binding [*being-expanded* (conj *being-expanded* macro-id)]
    (expand-macros (macros macro-id) macros)))

(defn expand-macros
  "Walk where clause and expand macros."
  [where macros]
  (walk/postwalk
   (fn [form]
     (if-not (vector? form)
       form
       (let [[clause-type _ :as clause] form]
         (if (= :macro clause-type)
           (expand-macros* clause macros)
           clause))))
   where))

(def simplify-clause* nil)
(defmulti ^:private simplify-clause*
  (fn [clause] (when (vector? clause) (clause 0)))
  :default nil)

(defmethod simplify-clause* nil
  [clause]
  clause)

(defn simplify-and-or [clause type]
  (reduce (fn [acc clause*]
            (if (and (vector? clause*)
                     (= type (first clause*)))
              (into acc (rest clause*))
              (conj acc clause*)))
          [type]
          (rest clause)))

(defmethod simplify-clause* :and [clause] (simplify-and-or clause :and))

(defmethod simplify-clause* :or [clause] (simplify-and-or clause :or))

(defmethod simplify-clause* :not [clause]
  (if (and (vector? (second clause))
           (= :not (-> clause second first)))
    (-> clause second second vec)
    clause))

(defn simplify-clause [clause]
  (walk/postwalk simplify-clause* clause))