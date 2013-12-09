(ns bitconj.util
  "Utility functions."
  (:use [clojure.string :only [join]]
        [clojure.math.numeric-tower :only [floor expt]]))

(defn bytes->uint
  "Returns the positive integer corresponding to the given big-endian bytes."
  [bytes]
  (BigInteger. 1 bytes))

(defn uint->bytes
  "Returns a big-endian representation of the given positive integer."
  [n]
  ;; BigInteger.toByteArray returns two's-complement representation,
  ;; so strip off a potential disambiguating leading zero byte
  ;; (we never deal with negative numbers)
  (->> (biginteger n) .toByteArray (drop-while zero?) byte-array))

(defn to-base
  "Takes a base-10 integer N and converts it to base B."
  [n b chars]
  (if (zero? n)
    (first chars)
    (loop [remainder n result []]
      (if (zero? remainder)
        (join (reverse result))
        (recur (floor (/ remainder b))
               (conj result (nth chars (mod remainder b))))))))

(defn from-base
  "Takes a base-B string S and converts it to base 10."
  [s b chars]
  (let [digits (into {} (map-indexed #(vector %2 (bigint %1)) chars))
        place-value (fn [i d] (* (get digits d)
                                (expt b (- (count s) (inc i)))))]
    (reduce + (map-indexed place-value s))))

(defn bytes-to-base
  "Like to-base, but takes a byte array as input and retains leading
  zero bytes in the form of leading zeroes in the result string."
  [bytes b chars]
  (let [b-zero (first chars)
        leading-zeroes (take-while zero? bytes)]
    (apply str (concat (map (fn [x] b-zero) leading-zeroes)
                       (to-base (bytes->uint bytes) b chars)))))

(defn bytes-from-base
  "Like from-base, but returns a byte array and retains leading zeroes
  as zero bytes in the front of the array."
  [s b chars]
  (let [b-zero? (fn [d] (= d (first chars)))
        leading-zeroes (take-while b-zero? s)]
    (byte-array (concat (map (fn [x] (byte 0)) leading-zeroes)
                        (uint->bytes (from-base s b chars))))))

(defn hexlify [bytes]
  (apply str (map #(format "%02X" %) bytes)))

(defn unhexlify [text]
  (letfn [(unhexlify-pair [[x y]]
            (unchecked-byte (Integer/parseInt (str x y) 16)))]
    (byte-array (map unhexlify-pair (partition 2 text)))))
