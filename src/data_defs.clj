(ns data-defs)

;param_count: 2 arg_set: #{["a","b"], ["b","c"], ..(de 2 params en este caso!)}
(defrecord Fact [name param_count arg_set])

;param_count: 1 calls: [Call_1, Call_2, ..]
(defrecord Rule [name param_count calls])

;arg_count: 3 args_index_or_literal: [0, 2, 1] o [1, 0, "pepe"]
(defrecord Call [name arg_count args_index_or_literal])


(defprotocol Transformable
  (transform [this items] "Transforma el dato a uno que puede ser evaluado."))


;name: "fact_name" params: ["a","b", ..]
(defrecord RawFact [name params]
  Transformable
  (transform [this raw_facts]
    (let [arg_set (into (hash-set) (map #(.params %) raw_facts))]
      (Fact. name (count params) arg_set))))

;name: "rule_name" params: ["a","b", ..] raw_calls: [RawCall_1, RawCall_2, ..]
(defrecord RawRule [name params raw_calls]
  Transformable
  (transform [this _]
    (let [indices (into (hash-map) (map-indexed #(vector (keyword %2), %1) params))
          calls (map #(transform % indices) raw_calls)]
      (Rule. name (count params) calls))))

;name: "call_name" args: ["a","b", ..]
(defrecord RawCall [name args]
  Transformable
  (transform [this indices]
    (let [arg_indices (map #(get indices (keyword %) %) args)]
      (Call. name (count args) arg_indices))))
