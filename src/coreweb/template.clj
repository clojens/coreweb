(ns coreweb.template
  (:use coreweb.tag
        coreweb.special
        coreweb.destructuring-vector.core
        coreweb.destructuring-vector.helper)
  (:require [clojure.java.io :refer [reader]]))

(defn- read-char [^java.io.Reader reader]
  (schar (.read reader)))

(defn- parse [reader]
  (let [rr #(read-char reader)
        parse-html-text (sd<-d (d-fn [c cur all]
                                 (cond
                                   (= nil c) [nil :text cur all]
                                   (= \< c) [(rr) :clj? [] (conj all (apply str cur))]
                                   :else (v-recur (rr) (conj cur c) all))))
        parse-html (sd<-d (d-fn [c s2 cur all]
                            (cond
                              (= nil c) [nil :html s2 cur all]
                              (= :text s2) (recur (parse-html-text c cur all))
                              (= :clj? s2) (cond
                                             (= \@ c) [(rr) :clj :code [] all]
                                             :else (v-recur (rr) :text (conj cur \< c) all)))))
        parse-clj-code (sd<-d (d-fn [c cur all]
                                (cond
                                  (= nil c) [nil :code cur all]
                                  (= \" c) [(rr) :str (conj cur c) all]
                                  (= \\ c) [(rr) :char (conj cur c) all]
                                  (= \@ c) [(rr) :html? cur all]
                                  :else (v-recur (rr) (conj cur c) all))))
        parse-clj-str (sd<-d (d-fn [c cur all]
                               (cond
                                 (= nil c) [nil :str cur all]
                                 (= \" c) [(rr) :code (conj cur c) all]
                                 (= \\ c) [(rr) :escape (conj cur c) all]
                                 :else (v-recur (rr) (conj cur c) all))))
        parse-clj (sd<-d (d-fn [c s2 cur all]
                           (cond
                             (= nil c) [nil :clj s2 cur all]
                             (= :code s2) (recur (parse-clj-code c cur all))
                             (= :str s2) (recur (parse-clj-str c cur all))
                             (= :char s2) (v-recur (rr) :code (conj cur c) all)
                             (= :html? s2) (cond
                                             (= \> c) [(rr) :html :text [] (conj all (read-string (apply str cur)))]
                                             :else (v-recur (rr) :code (conj cur \@ c) all))
                             (= :escape s2) (v-recur (rr) :str (conj cur c) all))))]
    (loop [[c s1 s2 cur all] [(rr) :html :text [] []]]
      (cond
        (= nil c) (conj all (apply str cur))
        (= :html s1) (recur (parse-html c s2 cur all))
        (= :clj s1) (recur (parse-clj c s2 cur all))))))

(defn do-fill [reader]
  (let [html (parse reader)]
    `(fn [~'request] (str ~@html))))

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