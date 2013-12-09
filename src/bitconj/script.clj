(ns bitconj.script
  (:use [clojure.tools.trace])
  (:require [bitconj.crypto :as crypt]))

(def make-stack list)

(defn push-stack [stack & items]
  (if (seq items) (apply conj stack items) stack))

(defn pop-stack [stack n]
  [(take n stack) (apply make-stack (drop n stack))])

(defn ripemd160 [[bytes] rest]
  (crypt/ripemd160 bytes))

(defn checksig [[signature pubkey] rest]
  [true])

(defn dup [[value] stack]
  [value value])

(defn truthify [x]
  (cond (= x true) 1
        (= x false) 0
        (nil? x) 0
        true x))

(defn wrap-op [f]
  (fn [args stack]
    (vector (truthify (apply f args)))))

(defn within [x min max]
  (and (>= x min) (< x max)))

(def nonzero? (complement zero?))

(defn by-name [operations]
  (into {} (map (juxt :name identity) operations)))

(def operations
  [;; stack
   {:name :toaltstack :opcode 107 :arity 1} ;; todo
   {:name :fromaltstack :opcode 108 :arity 1} ;; todo
   {:name :ifdup :opcode 115 :arity 1 :fn (fn [[val] rest] ((if zero? val) [val] [val val]))}
   {:name :depth :opcode 116 :arity 0 :fn (fn [stack] [(count stack)])}
   (:name :drop :opcode 117 :arity 1 :fn (fn [stack] []))
   ;; splice
   {:name :cat :opcode 126 :arity 2 :enabled false}
   {:name :substr :opcode 127 :arity 3 :enabled false}
   {:name :left :opcode 128 :arity 2 :enabled false}
   {:name :right :opcode 129 :arity 2 :enabled false}
   {:name :size :opcode :130 :arity 1 :fn (wrap-op count)}
   ;; logic
   {:name :invert :opcode 131 :arity 1 :enabled false}
   {:name :and :opcode 132 :arity 1 :enabled false}
   {:name :or :opcode 133 :arity 1 :enabled false}
   {:name :xor :opcode 134 :arity 1 :enabled false}
   {:name :equal :opcode 135 :arity 2 :fn (wrap-op =)}
   {:name :equalverify :opcode 136 :arity 2} ;; todo
   ;; arithmetic
   {:name :1add :opcode 139 :arity 1 :fn (wrap-op inc)}
   {:name :1sub :opcode 140 :arity 1 :fn (wrap-op dec)}
   {:name :2mul :opcode 141 :arity 1 :enabled false}
   {:name :2div :opcode 142 :arity 1 :enabled false}
   {:name :negate :opcode 143 :arity 1 :fn (wrap-op -)}
   {:name :abs :opcode 144 :arity 1 :fn (wrap-op #(Math/abs %))}
   {:name :not :opcode 145 :arity 1 :fn (wrap-op zero?)}
   {:name :0notequal :opcode 146 :arity 1 :fn (wrap-op nonzero?)}
   {:name :add :opcode 147 :arity 2 :fn (wrap-op +)}
   {:name :sub :opcode 148 :arity 2 :fn (wrap-op -)}
   {:name :mul :opcode 149 :arity 2 :enabled false}
   {:name :div :opcode 150 :arity 2 :enabled false}
   {:name :mod :opcode 151 :arity 2 :enabled false}
   {:name :lshift :opcode 152 :arity 2 :enabled false}
   {:name :rshift :opcode 153 :arity 2 :enabled false}
   {:name :booland :opcode 154 :arity 2 :fn (wrap-op #(and (nonzero? %1) (nonzero? %2)))}
   {:name :boolor :opcode 155 :arity 2 :fn (wrap-op #(or (nonzero? %1) (nonzero? %2)))}
   {:name :numequal :opcode 156 :arity 2 :fn (wrap-op =)}
   {:name :numequalverify :opcode 157 :arity 2} ;; todo
   {:name :numnotequal :opcode 158 :arity 2 :fn (wrap-op not=)}
   {:name :lessthan :opcode 159 :arity 2 :fn (wrap-op <)}
   {:name :greaterthan :opcode 160 :arity 2 :fn (wrap-op >)}
   {:name :lessthanorequal :opcode 161 :arity 2 :fn (wrap-op <=)}
   {:name :greaterthanorequal :opcode 162 :arity 2 :fn (wrap-op >=)}
   {:name :min :opcode 163 :arity 2 :fn (wrap-op min)}
   {:name :max :opcode 164 :arity 2 :fn (wrap-op max)}
   {:name :within :opcode 165 :arity 3 :fn (wrap-op within)}
   ])))


(defn apply-op [operation stack]
  (let [[f arity] ((juxt :fn :arity) operation)
        [operands rest] (pop-stack stack arity)]
    (apply push-stack rest (f operands stack))))

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
