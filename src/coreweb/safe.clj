(ns coreweb.safe)

(defn translate [f obj pred]
  (if-let [v (pred obj)]
    (assoc obj pred (f v))
    obj))

(defn translate-all [f obj]
  (reduce #(assoc %1 %2 (f (%1 %2))) obj (keys obj)))

(defn safe-html [s]
  (clojure.string/replace s #"<|>" {"<" "&lt;" ">" "&gt;"}))

(def safe (partial translate safe-html))

(def safe-all (partial translate-all safe-html))