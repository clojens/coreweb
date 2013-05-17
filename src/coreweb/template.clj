(ns coreweb.template
  (:use coreweb.tag
        coreweb.special)
  (:require [clojure.java.io :refer [reader]]))

(defn- rc [^java.io.Reader reader]
  (schar (.read reader)))

(defn- parse [r]
  (loop [c (rc r) s1 :html s2 :text cur [] all []]
    (cond
      (= nil c) (conj all (apply str cur))
      (= :html s1) (cond
                     (= :text s2) (cond
                                    (= \< c) (recur (rc r) s1 :clj? [] (conj all (apply str cur)))
                                    :else (recur (rc r) s1 s2 (conj cur c) all))
                     (= :clj? s2) (cond
                                    (= \@ c) (recur (rc r) :clj :code [] all)
                                    :else (recur (rc r) s1 :text (conj cur \< c) all)))
      (= :clj s1) (cond
                    (= :code s2) (cond
                                   (= \" c) (recur (rc r) s1 :str (conj cur c) all)
                                   (= \\ c) (recur (rc r) s1 :char (conj cur c) all)
                                   (= \@ c) (recur (rc r) s1 :html? cur all)
                                   :else (recur (rc r) s1 s2 (conj cur c) all))
                    (= :str s2) (cond
                                  (= \" c) (recur (rc r) s1 :code (conj cur c) all)
                                  (= \\ c) (recur (rc r) s1 :escape (conj cur c) all)
                                  :else (recur (rc r) s1 s2 (conj cur c) all))
                    (= :char s2) (recur (rc r) s1 :code (conj cur c) all)
                    (= :html? s2) (cond
                                    (= \> c) (recur (rc r) :html :text [] (conj all (read-string (apply str cur))))
                                    :else (recur (rc r) s1 :code (conj cur \@ c) all))
                    (= :escape s2) (recur (rc r) s1 :str (conj cur c) all)))))

(defn do-fill [reader]
  (let [html (parse reader)]
    `(fn [~(symbol "request")] (str ~@html))))

(defprotocol fillable
  (fill [input]
    (do-fill (reader (.getBytes (str input))))))

(extend-protocol fillable
  nil
  (fill [_] nil)
  java.io.Reader
  (fill [reader] (do-fill reader))
  java.io.InputStream
  (fill [in] (do-fill (reader in)))
  java.io.File
  (fill [file] (let [r (reader file) f (do-fill r)] (.close r) f))
  java.net.URI
  (fill [uri] (let [r (reader uri) f (do-fill r)] (.close r) f))
  java.net.URL
  (fill [url] (let [r (reader url) f (do-fill r)] (.close r) f))
  java.net.Socket
  (fill [socket] (do-fill (reader socket)))
  String
  (fill [file-name] (let [r (reader file-name) f (do-fill r)] (.close r) f)))