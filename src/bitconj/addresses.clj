(ns bitconj.addresses
  "Tools for working with Bitcoin addresses."
  (:use [bitconj.util :only [unhexlify hexlify to-base58]]
        [bitconj.crypto :only [ripemd160 sha256]]))

(def address-version-byte (byte 0))

(defn bytes->b58 [payload version]
  (let [data (byte-array (cons version payload))
        checksum (->> data sha256 sha256 (take 4))
        [leading-zeroes rest] (split-with zero? data)]
    (apply str (concat (map to-base58 leading-zeroes)
                       (to-base58 (bytes->int (byte-array (concat rest checksum))))))))

(defn address-from-public-key [key]
  (-> key sha256 ripemd160 (bytes->b58 address-version-byte)))
