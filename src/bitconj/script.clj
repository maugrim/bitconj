(ns bitcoin.script
  (:use clojure.tools.trace))

(def make-stack list)
(def push-stack conj)
(defn pop-stack [stack n]
  [(take n stack) (apply make-stack (drop n stack))])

(defn ripemd160 [data]
  [data])

(defn checksig [signature pubkey]
  [true])

(defn dup [value]
  [value value])

(def operations
  (into {} (map (juxt :opcode identity)
   [{:opcode :dup :name "OP_DUP" :arity 1 :fn dup}
    {:opcode :checksig :name "OP_CHECKSIG" :arity 2 :fn checksig}
    {:opcode :hash160 :name "OP_HASH160" :arity 1 :fn ripemd160}
    {:opcode :eq :name "OP_EQ" :arity 2 :fn (comp vector =)}
    {:opcode :add :arity 2 :fn (comp vector +)}
    {:opcode :mul :arity 2 :fn (comp vector *)}])))

(defn apply-op [operation stack]
  (let [[f arity] ((juxt :fn :arity) operation)
        [operands rest] (pop-stack stack arity)]
    (apply push-stack rest (apply f operands))))

(defn process
  [script stack]
  (if-let [item (first script)]
    (if-let [op (get operations item)]
      (process (next script) (apply-op op stack))
      (process (next script) (push-stack stack item)))
    stack)))

(defn bitcoin.script/eval
  ([script] (process script (make-stack)))
  ([script stack] (process script stack)))
