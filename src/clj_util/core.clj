(ns clj-util.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defmacro debug
  [& body]
  `(let [val# ~@body]
     (println val#)
     val#))