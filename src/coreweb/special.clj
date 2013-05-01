(ns coreweb.special
  (:use ring.util.codec))

(defn uri-encode [uri]
  (url-encode (clojure.string/replace uri \? \？)));for clojure symbol

(defn uri-decode [uri]
  (clojure.string/replace (url-decode uri) \？ \?));for clojure symbol