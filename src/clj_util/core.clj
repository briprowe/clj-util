(ns clj-util.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.inspect :as inspect])
  (:import [java.util Arrays]))

(defn inspect-ns
  [ns]
  (inspect/inspect (into {} (map (fn [[k v]] [k (deref v)]) (ns-interns ns)))))

(defn encode-utf8
  [str]
  (.getBytes str "UTF8"))

(defn decode-utf8
  [bytes]
  (String. bytes "UTF8"))

(defn byte-cast [x]
  (let [y (bit-and 16r000000ff x)]
    (byte (if (> y 127) (+ -256 y) y))))

(defn to-bytes
  [seq]
  (byte-array (count seq) (map byte-cast seq)))

(let [values {\0 0, \1 1, \2 2, \3 3, \4 4, \5 5, \6 6,
              \7 7, \8 8, \9 9, \a 0xa, \b 0xb, \c 0xc,
              \d 0xd, \e 0xe, \f 0xf}]
  (defn make-byte
    "Return a java byte given two hexidecimal characters."
    [char1 char2]
    {:pre [(comp not nil? (get values char1))
           (comp not nil? (get values char2))]
     :post [(instance? java.lang.Byte %)]}
    (byte-cast (+ (* 16 (get values char1)) (get values char2)))))

(defn make-byte-array
  "Make a byte array given a hexidecimal string."
  [s]
  (byte-array (map #(make-byte (first %) (second %))
                   (partition 2 s))))

(defn read-key-value-stream
  "Read a stream consisting of a series of \"key=value\n\" and
return a map containing those pairs. "
  [input]
  (with-open [rdr (io/reader input)]
    (loop [line (.readLine rdr)
           retval {}]
      (if line
        (let [[k v] (str/split line #"=")
              k (str/trim k)
              v (str/trim v)]
          (recur (.readLine rdr) (merge retval {k v})))
        retval))))

(defn array=
  [as bs]
  (Arrays/equals as bs))

(defmacro debug
  [& body]
  `(let [val# ~@body]
     (println val#)
     val#))