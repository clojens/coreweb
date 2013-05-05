(ns coreweb.tag)

(defn joiner [s]
  (partial clojure.string/join s))

(defn tag
  ([tag-name attrs]
    (str "<" tag-name
      ((joiner " ") (cons "" (map (fn [[k v]] (str k "=" \" v \")) attrs)))
      ">"))
  ([tag-name]
    (str "<" tag-name ">")))

(def tag-fns
  {:open tag
   :close (fn [tag-name attrs & content]
            (str (tag tag-name attrs) (apply str content) "</" tag-name ">"))})

(def tag-names
  {:open ["input" "br"]
   :close ["a" "form" "textarea" "html" "body"]});need more

(defmacro deftag [tag-name tag-fn]
  `(def ~(symbol tag-name)
     (partial ~tag-fn ~tag-name)))

(defmacro define-tags [tag-type]
  `(do ~@(map (fn [x#] `(deftag ~x# ~(tag-fns tag-type))) (tag-names tag-type))))

(defmacro define-all-tags []
  `(do ~@(map (fn [x#] `(define-tags ~x#)) (keys tag-fns))))

(define-all-tags)