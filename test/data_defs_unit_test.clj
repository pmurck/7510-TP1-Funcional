(ns data-defs-unit-test
  (:require [clojure.test :refer :all]
            [data-defs :refer :all])
  (:import (data_defs Fact Rule Call RawFact RawRule RawCall)
           (clojure.lang ExceptionInfo)))


(def rf_1 (RawFact. "padre" ["a","b"]))
(def rf_2 (RawFact. "padre" ["a","z"]))
(def rc_1 (RawCall. "varon" ["x","y"]))
(def rc_2 (RawCall. "varon" ["y","z"]))
(def rr (RawRule. "rule" ["x","y","z"] [rc_1, rc_2]))


(deftest transform_raw_fact_test
  (testing "Get Fact from transform RawFacts"
    (is (instance? Fact (transform rf_1 [rf_1, rf_2]))))

  (testing "Check Fact has correct param_count"
    (is (= (.param_count (transform rf_1 [rf_1, rf_2])) 2)))

  (testing "Check Fact has correct arg_set"
    (is (every? true? (map #(contains? (.arg_set (transform rf_1 [rf_1, rf_2])) %) [(.params rf_1), (.params rf_2)])))))


(deftest transform_raw_call_test
  (testing "Get Call from transform RawCall"
    (is (instance? Call (transform rc_1 {:x 0 :y 1}))))

  (testing "Check Call has correct param_count"
    (is (= (.arg_count (transform rc_1 {:x 0 :y 1})) 2)))

  (testing "Check Call has correct args replaced"
    (is (= (.args_index_or_literal (transform rc_1 {:x 0 :y 1})) [0,1]))))


(deftest transform_raw_rule_test
  (testing "Get Rule from transform RawRule"
    (is (instance? Rule (transform rr nil))))

  (testing "Check Rule has correct param_count"
    (is (= (.param_count (transform rr nil)) 3)))

  (testing "Check Rule has list of Calls"
    (is (every? #(instance? Call %) (.calls (transform rr nil))))))
