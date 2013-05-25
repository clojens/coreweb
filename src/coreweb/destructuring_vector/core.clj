(ns coreweb.destructuring-vector.core
  (:use coreweb.destructuring-vector.helper))

(defmacro d-fn [need-vector & more]
  (if (vector? need-vector)
    `(fn [~need-vector] ~@more)
    (throw (Exception. "Unsupport overloads."))))

(defmacro d-defn [n doc? & more]
  (cond
    (string? doc?) (let [arg (first more) body (rest more)]
                     (if (vector? arg)
                       `(defn ~n ~doc? [~arg] ~@body)
                       (throw (Exception. "the arglist must be a vector."))))
    (vector? doc?) `(defn ~n [~doc?] ~@more)
    :else (throw (Exception. "second argument must be a string or vector."))))

(defmacro dv-fn [need-vector & more]
  (if (vector? need-vector)
    `(fn [~need-vector] [~@more])
    (throw (Exception. "Unsupport overloads."))))

(defmacro dv-defn [n doc? & more]
  (cond
    (string? doc?) (let [arg (first more) body (rest more)]
                     (if (vector? arg)
                       `(defn ~n ~doc? [~arg] [~@body])
                       (throw (Exception. "the arglist must be a vector."))))
    (vector? doc?) `(defn ~n [~doc?] [~@more])
    :else (throw (Exception. "second argument must be a string or vector."))))

(defmacro dvo-fn [need-vector & more]
  (if (vector? need-vector)
    `(fn [~need-vector] [(do ~@more)])
    (throw (Exception. "Unsupport overloads."))))

(defmacro dvo-defn [n doc? & more]
  (cond
    (string? doc?) (let [arg (first more) body (rest more)]
                     (if (vector? arg)
                       `(defn ~n ~doc? [~arg] [(do ~@body)])
                       (throw (Exception. "the arglist must be a vector."))))
    (vector? doc?) `(defn ~n [~doc?] [(do ~@more)])
    :else (throw (Exception. "second argument must be a string or vector."))))


(auto d<-fn "d-" clojure.core)

(auto dvo<-fn "dvo-" clojure.core)

(defmacro v-recur [& body]
  `(recur [~@body]))

