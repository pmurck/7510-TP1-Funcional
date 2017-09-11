(ns parser_unit_test
  (:require [clojure.test :refer :all]
            [parser :refer :all]
            [data-defs])
  (:import (data_defs RawFact RawRule RawCall)
           (clojure.lang ExceptionInfo)))

(defn test_parse [method_def, expected]
  (testing (clojure.string/join [method_def, "should give", expected] " ")
    (is (= (parse method_def) expected))))


(defn test_bad_parse [method_def]
  (testing (clojure.string/join [method_def, "should throw Exception"] " ")
    (is (thrown? ExceptionInfo (parse method_def)))))


(deftest syntactically-correct-method-parse
  (test_parse "temp(one, two)" ["temp" ["one", "two"]])
  (test_parse "t(o,t)" ["t", ["o", "t"]])
  (test_parse "   t    (      o   , t   )     " ["t", ["o", "t"]])
  (test_parse "_hola_(x)" ["_hola_" ["x"]])
  (test_parse "empty_args()" ["empty_args", []]))


(deftest syntactically-incorrect-method-parse
  (test_bad_parse "(x,y)")
  (test_bad_parse "(x,)")
  (test_bad_parse "(,)")
  (test_bad_parse "()")
  (test_bad_parse "(x)")
  (test_bad_parse "")
  (test_bad_parse "hola")
  (test_bad_parse "x(")
  (test_bad_parse "y)")
  (test_bad_parse "(x")
  (test_bad_parse "hola(x,"))


(deftest successful_parse_line_to_type
  (testing "Get RawFact from fact def: hola(pepe,juan)"
    (is (instance? RawFact (parse_line "hola(pepe,juan)"))))

  (testing "Get RawRule from rule def: self_saludo(x) :- hola(x,x)"
    (is (instance? RawRule (parse_line "self_saludo(x) :- hola(x,x)"))))

  (testing "RawRule contains RawCalls from rule def: self_saludo(x) :- hola(x,x)"
    (is (every? #(instance? RawCall %) (.raw_calls (parse_line "self_saludo(x) :- hola(x,x)"))))))


(deftest unsuccessful_parse_line_to_type
  (testing "Incorrect fact def: hola(pepe,) throws Exception"
    (is (thrown? ExceptionInfo (parse_line "hola(pepe,)"))))

  (testing "Incorrect rule def: self:-() throws Exception"
    (is (thrown? ExceptionInfo (parse_line "self:-()")))))


(deftest successful_parse_db
  (testing "Get RawFacts from db: \"hola(pepe,juan). chau(juan, pepe)\""
    (is (every? #(instance? RawFact %) (parse_db "hola(pepe,juan). chau(juan, pepe)."))))

  (testing "Get RawRules from db: \"r(x) :- hola(x,x). y(z,x) :- i(x), a(z).\""
    (is (every? #(instance? RawRule %) (parse_db "r(x) :- hola(x,x). y(z,x) :- i(x), a(z).")))))
