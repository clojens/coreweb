(ns coreweb.destructuring-vector.helper)

(defmacro d<-fn [f]
  `(fn [args#]
     (apply ~f args#)))

(defmacro dvo<-fn [f]
  `(fn [args#]
     [(apply ~f args#)]))

(defmacro sd<-d [df]
  `(fn sd-fn#
     ([args#] (~df args#))
     ([head# & tail#] (sd-fn# (vec (cons head# tail#))))))

(def macro? (comp true? :macro meta))

(defmacro auto [transform prefix ans]
  `(do ~@(map (fn [[s#]]
                (let [n# (symbol (clojure.string/replace (str prefix s#) "/" "|"))]
                  `(def ~n# (~transform ~s#))))
           (filter #(and (fn? @(second %)) (not (macro? (second %)))) (ns-publics ans)))))