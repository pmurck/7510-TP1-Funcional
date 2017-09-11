(ns edge-case-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def parent-database "
	varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
	hijo_de_juan(x) :- varon(x), padre(juan, x).
	hija_de_hector(alguien) :- hija(alguien, hector).
	error(A, B, C) :- error(C, B, A).
	wrapper(X, x) :- hijo(X, x).
	varon(juan, pepe).
	varon(X, Y, Z) :- varon(X, Y), varon(Z).
	unused(X, UnUsed) :- varon(X).
	empty_rule() :- varon(juan).
")

(deftest rule-with-fixed-arg-test
  (testing "hijo_de_juan(pepe) should be true"
    (is (= (evaluate-query parent-database "hijo_de_juan(pepe)")
           true))))


(deftest rule-calls-another-rule-test
  (testing "hijo_de_hector(maria) should be true"
    (is (= (evaluate-query parent-database "hija_de_hector(maria)")
           true))))


(deftest rule-params-are-case-sensitive-test
  (testing "rules are case sensitive"
    (is (= (evaluate-query parent-database "wrapper(pepe, juan)")
           true))))


(deftest same-name-rule-and-fact-accepted-if-different-param-count-test
  (testing "Rules and facts with same name are OK if they have different parameter count"
    (is (= (evaluate-query parent-database "varon(juan, pepe, juan)")
           true))))


(deftest unused-paramaters-in-rule-accepted-test
  (testing "Rules can have unused parameters"
    (is (= (evaluate-query parent-database "unused(juan, zaraza)")
           true))))


(deftest rules-can-have-no-params-accepted-test
  (testing "Rules can have zero parameteres"
    (is (= (evaluate-query parent-database "empty_rule()")
           true))))


(deftest fact-with-no-arg-is-accepted-test
  (testing "Fact with no argument is OK"
    (is (= (evaluate-query "OK_fact()." "OK_fact()")
           true))))


(deftest rule-calls-nonexistent-method-is-an-error-test
  (testing "Rule with calls that point to nonexistent rules or fact is an error"
    (is (= (evaluate-query "bad_rule(x) :- no_fact(x)." "bad_rule(zaraza)")
           nil))))


(deftest rule-calls-itself-is-an-error-test
  (testing "Rule calling itself is an error, and should be nil"
    (is (= (evaluate-query parent-database "error(test,this,error)")
           nil))))
