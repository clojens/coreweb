(ns coreweb.special
  (:use ring.util.codec))

(defn uri-encode [uri]
  (url-encode (clojure.string/replace uri \? \？)));for clojure symbol

(defn uri-decode [uri]
  (clojure.string/replace (url-decode uri) \？ \?));for clojure symbol

(defn str-all [obj]
  (cond
    (map? obj) (str "{" (clojure.string/join ", " (map (fn [[l r]] (str (str-all l) " " (str-all r))) obj)) "}")
    (vector? obj) (str "[" (clojure.string/join ", " (map str-all obj)) "]")
    (set? obj) (str "#{" (clojure.string/join ", " (map str-all obj)) "}")
    (seq? obj) (str "(" (clojure.string/join ", " (map str-all obj)) ")")
    :else (str obj)))

(defn str-coll [obj]
  (if (seq? obj)
    (str-all obj)
    obj))