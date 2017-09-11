(ns data-defs)

;name: "fact_name" params: ["a","b", ..]
(defrecord RawFact [name params])

;name: "rule_name" params: ["a","b", ..] raw_calls: [RawCall_1, RawCall_2, ..]
(defrecord RawRule [name params raw_calls])

;name: "call_name" args: ["a","b", ..]
(defrecord RawCall [name args])
