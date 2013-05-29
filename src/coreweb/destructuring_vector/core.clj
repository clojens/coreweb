(ns coreweb.destructuring-vector.core
  (:use coreweb.destructuring-vector.helper))

(build-all coreweb.destructuring-vector.helper/build-higher-order-fn)

(build-all coreweb.destructuring-vector.helper/build-higher-order-defn)

(auto clojure.core "d-" d<-fn)

(auto clojure.core "dvo-" dvo<-fn)

(defmacro v-recur [& body]
  `(recur [~@body]))

(defn dd-partial [[f & more]]
  (fn [v]
    (f (lazy-cat more v))))