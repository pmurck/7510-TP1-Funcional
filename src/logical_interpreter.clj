(ns logical-interpreter
  (:require [data-defs :refer :all] [parser :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defn evaluate-query
  "Returns true if the rules and facts in database imply query, false if not. If
  either input can't be parsed, returns nil"
  [database str_query]
  (try
    (let [[q_name q_args] (parse str_query)
          raw_db (parse_db database)
          db (transform_db raw_db)
          evaluable (query db q_name (count q_args))]
      (evaluate evaluable q_args (ref db)))
    (catch ExceptionInfo e
      (println (ex-data e))))) ;println devuelve nil
