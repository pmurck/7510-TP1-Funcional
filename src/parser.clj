(ns parser
  (:require [clojure.string])
  (:import (data_defs RawCall RawFact RawRule)))

(def ^:const method_regex #"(\s*\w+\s*)\((\s*[^,\s()]*\s*(?:,\s*[^,\s()]+\s*)*)\)\s*")
(def ^:const rule_symbol_regex #":-")
(def ^:const rule_regex #"(.*):-(.*)")
(def ^:const line_end_symbol #"\.")
(def ^:const arg_delimiter #",")
(def ^:const call_delim_regex #"\)\s*,")
(def ^:const call_delim_replacement ");")
(def ^:const call_delimiter #";")


(defn- split_trim_and_filter_blank [string, split_regex]
  "Separa el string según split_regex. Luego,
  filtra y limpia el whitespace."
  (let [split_strings (clojure.string/split string split_regex)
        trimmed_strings (map clojure.string/trim split_strings)
        blank_filtered_strings (filter #(not (clojure.string/blank? %)) trimmed_strings)]
    blank_filtered_strings))


(defn- try_re-matches [regex, string]
  "En caso de matchear el string usando regex, lo devuelve.
  Si no matchea, lanza una excepcion informativa."
  (let [result (re-matches regex string)]
    (if (nil? result)
      (throw (ex-info "Syntax error" {:error "Syntax error" :culprit string}))
      result)))


(defn parse [method]
  "Parsea el metodo y lo separa en [nombre [arg1, arg2, etc]]."
  (let [[_ name args_string] (try_re-matches method_regex method)
        trimmed_name (clojure.string/trim name)
        args (split_trim_and_filter_blank args_string arg_delimiter)]
    (vector trimmed_name, args)))


(defn- parse_calls [rule_body]
  "Parsea y crea RawCalls del rule_body."
  (let [delimited_calls (clojure.string/replace rule_body call_delim_regex call_delim_replacement)
        calls (split_trim_and_filter_blank delimited_calls call_delimiter)]
    (map #(let [[name args] (parse %)]
            (RawCall. name args))
         calls)))


"Parsea la linea y devuelve el tipo de dato que representa esa linea."
(defmulti parse_line #(re-find rule_symbol_regex %))

(defmethod parse_line :default [rule_line]
  "Parsea la linea y devuelve un RawRule."
  (let [[_ method body] (try_re-matches rule_regex rule_line)
        [name params] (parse method)
        raw_calls (parse_calls body)]
    (RawRule. name params raw_calls)))

(defmethod parse_line nil [fact_line]
  "Parsea la linea y devuelve un RawFact."
  (let [[name params] (parse fact_line)]
    (RawFact. name params)))


(defn parse_db [db]
  "Parsea la base de datos, generando una lista de RawCalls y RawFacts."
  (let [lines (split_trim_and_filter_blank db line_end_symbol)]
    (map parse_line lines)))
