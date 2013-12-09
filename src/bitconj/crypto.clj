(ns bitconj.crypto
  "A wrapper for the Java libraries that do all the real crypto work."
  (:import [org.bouncycastle.crypto.digests RIPEMD160Digest]
           [org.bouncycastle.jce ECNamedCurveTable]
           [org.bouncycastle.jce.provider BouncyCastleProvider]
           [org.bouncycastle.jce.spec ECParameterSpec ECPrivateKeySpec ECPublicKeySpec]
           [java.security KeyFactory KeyPair MessageDigest Security Signature]))

(Security/addProvider (BouncyCastleProvider.))

(def curve-name "secp256k1")
(def curve-params (ECNamedCurveTable/getParameterSpec curve-name))
(def key-factory (KeyFactory/getInstance "ECDSA" "BC"))

(defn d->keypair
  "Generates an EC keypair from the 256-bit private key d."
  [d]
  (let [private (ECPrivateKeySpec. (biginteger d) curve-params)
        public (ECPublicKeySpec. (.multiply (.getG curve-params) (biginteger d)) curve-params)]
    (KeyPair. (.generatePublic key-factory public)
              (.generatePrivate key-factory private))))

(defn private-key [key-pair]
  (.getPrivate key-pair))

(defn public-key [key-pair]
  (.getPublic key-pair))

(defn- make-signer []
  (Signature/getInstance "NONEwithECDSA" "BC"))

(defn sign-using [bytes private-key]
  (let [ecdsa (make-signer)]
    (.initSign ecdsa private-key)
    (.update ecdsa bytes)
    (.sign ecdsa)))

(defn sign-verify [bytes signature public-key]
  (let [ecdsa (make-signer)]
    (.initVerify ecdsa public-key)
    (.update ecdsa bytes)
    (.verify ecdsa signature)))

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
