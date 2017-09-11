(ns data-defs)


(defprotocol Queryable
  (query [this method_name param_count] "Devolver el Evaluable con esas propiedades"))


;data: {:method_name1 {:param_count1 Evaluable :param_count2 Evaluable} :method_name2 {..} }
(defrecord DB [data]
  Queryable
  (query [this method_name param_count]
    (let [k_name (keyword method_name)
          k_param_count (keyword (str param_count))
          evaluable (get-in data [k_name, k_param_count])]
      (if (nil? evaluable)
        (throw (ex-info "No such method defined"
                        {:cause "No such method defined" :method method_name :param_count param_count}))
        evaluable))))

;*******************************************************************************************************

(defprotocol Evaluable
  (evaluate [this args db_ref] "Evalua el tipo de dato con los argumentos especificados"))


;param_count: 2 arg_set: #{["a","b"], ["b","c"], ..(de 2 params en este caso!)}
(defrecord Fact [name param_count arg_set]
  Evaluable
  (evaluate [this args _]
    (contains? arg_set args)))

;param_count: 1 calls: [Call_1, Call_2, ..]
(defrecord Rule [name param_count calls]
  Evaluable
  (evaluate [this args db_ref]
    (if (some #(and (= name (.name %)) (= param_count (.arg_count %))) calls)
      (throw (ex-info "Rule can't call itself"
                      {:cause "Rule can't call itself" :rule_name name
                       :rule_param_count param_count :rule_calls calls}))
      (every? true? (map #(evaluate % args db_ref) calls)))))

;arg_count: 3 args_index_or_literal: [0, 2, 1] o [1, 0, "pepe"]
(defrecord Call [name arg_count args_index_or_literal]
  Evaluable
  (evaluate [this args db_ref]
    (let [evaluable (query (deref db_ref) name arg_count)
          replaced_args (map #(if (number? %) (nth args %) %) ;Check for literal
                             args_index_or_literal)]
      (evaluate evaluable replaced_args db_ref))))

;*******************************************************************************************************

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

;*******************************************************************************************************
; RawDB

(defn group_by_and_apply [item_list, group_fn, result_fn]
  "1° Agrupa item_list segun group_fn.
   2° Aplica result_fn a cada lista de agrupados.
   3° Introduce 1° y 2° en hash-map con 1° keywordeado."
  (into (hash-map)
        (map #(let [[group_key items] %]
                [(keyword group_key), (result_fn items)])
             (seq (group-by group_fn item_list)))))


(defn transform_db [raw_items]
  "Transforma los raw_items y genera la DB con el formato especificado arriba."
  (let [param_count_fn #(str (count (.params %)))
        transform_item #(transform (first %) %)]
    (DB. (group_by_and_apply raw_items
                             #(.name %)
                             #(group_by_and_apply % param_count_fn transform_item)))))
