(ns bitconj.util
  "Utility functions."
  (:use [clojure.string :only [join]]
        [clojure.math.numeric-tower :only [floor]]))

(defn bytes->int [bytes]
  (BigInteger. bytes))

(defn int->bytes [n]
  (.toByteArray (biginteger n)))

(defn to-base [n base chars]
  (if (zero? n)
    (first chars)
    (loop [remainder n result []]
      (if (zero? remainder)
        (join (reverse result))
        (recur (floor (/ remainder base))
               (conj result (nth chars (mod remainder base))))))))

(def b58-charset "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(defn to-base58 [n]
  (to-base n 58 b58-charset))

(defn hexlify [bytes]
  (apply str (map #(format "%02X" %) bytes)))

(defn unhexlify [text]
  (letfn [(unhexlify-pair [[x y]]
            (unchecked-byte (Integer/parseInt (str x y) 16)))]
    (byte-array (map unhexlify-pair (partition 2 text)))))
