(ns bitconj.addresses
  "Tools for working with Bitcoin addresses."
  (:use [clojure.set :only [map-invert]]
        [bitconj.util :only [bytes->uint uint->bytes bytes-to-base bytes-from-base]]
        [bitconj.crypto :only [ripemd160 sha256 d->keypair private-key public-key]])
  (:import [java.security GeneralSecurityException]))

(def checksum-size 4) ;; bytes

(def version-prefixes
  {:pay-to-address (byte 0)
   :pay-to-script-hash (byte 5)})

(def versions-by-prefix (map-invert version-prefixes))

(def b58-charset "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")

(defn- encode-b58 [bytes]
  (bytes-to-base bytes 58 b58-charset))

(defn- decode-b58 [s]
  (bytes-from-base s 58 b58-charset))

(defn- make-checksum [bytes]
  (-> bytes sha256 sha256))

(defn- make-contents [version payload]
  (byte-array (cons version payload)))

(defn split-at-end [n coll]
  (split-at (- (count coll) n) coll))

(defn bytes->b58 [prefix payload]
  (let [data (make-contents prefix payload)
        checksum (->> data make-checksum (take checksum-size))]
    (encode-b58 (byte-array (concat data checksum)))))

(defn b58->bytes [s]
  (let [[[prefix & payload] x-checksum] (split-at-end checksum-size (decode-b58 s))
        data (make-contents prefix payload)
        checksum (->> data make-checksum (take checksum-size))]
    (if (not= checksum x-checksum)
      (throw (GeneralSecurityException. "Base58Check checksum mismatch."))
      [prefix payload])))

(defn address-from-passphrase [text]
  (-> text .getBytes sha256 bytes->uint d->keypair public-key .getQ .getEncoded address-from-public-key))

(defn address-from-public-key [key]
  (->> key sha256 ripemd160 (bytes->b58 (:pay-to-address version-prefixes))))

(defn decode-address [address]
  (let [[prefix payload] (b58->bytes address)]
    {:payload payload
     :version (get versions-by-prefix prefix)}))
