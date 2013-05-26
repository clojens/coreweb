(ns coreweb.destructuring-vector.core
  (:use coreweb.destructuring-vector.helper))

(build-all coreweb.destructuring-vector.helper/build-higher-order-fn)

(build-all coreweb.destructuring-vector.helper/build-higher-order-defn)

(auto d<-fn "d-" clojure.core)

(auto dvo<-fn "dvo-" clojure.core)

(defmacro v-recur [& body]
  `(recur [~@body]))