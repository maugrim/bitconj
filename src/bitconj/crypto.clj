(ns bitconj.crypto
  "A wrapper for the Java libraries that do all the real crypto work."
  (:use [clojure.tools.trace]
        [bitconj.util])
  (:import [org.bouncycastle.crypto.digests RIPEMD160Digest]
           [org.bouncycastle.jce ECNamedCurveTable]
           [org.bouncycastle.jce.spec ECParameterSpec]
           [java.security KeyFactory KeyPair KeyPairGenerator MessageDigest SecureRandom]))

(def default-curve "secp256k1")

(defn ecdsa-pair [curve-name]
  (let [spec (ECNamedCurveTable/getParameterSpec curve-name)
        gen (KeyPairGenerator/getInstance "ECDSA", "BC")
        prng (SecureRandom.)]
    (. gen (initialize spec prng))
    (. gen (generateKeyPair))))

(defn ecdsa-from-pk []

  )

(defn private-key [key-pair]
  (.getPrivate key-pair))

(defn public-key [key-pair]
  (.getPublic key-pair))

(defn sha256 [bytes]
  (let [md (MessageDigest/getInstance "SHA-256")]
    (. md update bytes)
    (.digest md)))

(defn ripemd160 [bytes]
  (let [md (RIPEMD160Digest.)]
    (. md update bytes 0 (count bytes))
    (let [digest (byte-array (.getDigestSize md))]
      (.doFinal md digest 0)
      digest)))
