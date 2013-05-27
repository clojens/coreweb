(ns coreweb.destructuring-vector.helper)

(defn check-meta [finder]
  (comp true? finder meta))

(defn arg<-destructuring [avector]
  (vec (take-while #(not= :as %) avector)))

(defn destructured-helper [avector]
  (if (some #{'&} avector)
    `(into [~@(take-while #(not= '& %) avector)]
       ~(second (drop-while #(not= '& %) avector)))
    (arg<-destructuring avector)))

(defn ^{:higher-order-builder true :overload false :prefix "d-"}
  d-fn-body [avector more]
  `([~avector] ~@more))

(defn ^{:higher-order-builder true :overload false :prefix "dv-"}
  dv-fn-body [avector more]
  `([~avector] [~@more]))

(defn ^{:higher-order-builder true :overload false :prefix "dvo-"}
  dvo-fn-body [avector more]
  `([~avector] [(do ~@more)]))

(defn ^{:higher-order-builder true :overload true :prefix "l-"}
  l-fn-body [avector more]
  (let [arg (arg<-destructuring avector)]
    `(~arg (loop [~avector ~(destructured-helper avector)]
             (do ~@more)))))

(defmacro build-all [builder-builder]
  `(do ~@(map (fn [[_ v#]] `(~builder-builder ~v#))
           (filter (comp (check-meta :higher-order-builder ) second)
             (ns-publics 'coreweb.destructuring-vector.helper)))))

(defmacro build-higher-order-fn [builder]
  (let [ameta (meta builder)]
    `(defmacro ~(symbol (str (:prefix ameta) "fn")) [vector?# & more#]
       (cond
         (vector? vector?#) `(fn ~(~builder vector?# more#))
         (seq? vector?#) (if ~(:overload ameta)
                           `(fn ~(map ~builder (cons vector?# more#)))
                           `(throw (Exception. "Unsupport overloads.")))
         (and (symbol? vector?#) (vector? (first more#))) `(fn vector?# ~(~builder (first more#) (rest more#)))
         (and (symbol? vector?#) (seq? (first more#))) (if ~(:overload ameta)
                                                         `(fn ~vector?# ~(map ~builder more#))
                                                         `(throw (Exception. "Unsupport overloads.")))
         :else (throw (Exception. (str "Unkonwn args:" vector?# "|" more#)))))))

(defmacro build-higher-order-defn [builder]
  (let [ameta (meta builder)]
    `(defmacro ~(symbol (str (:prefix ameta) "defn")) [n# doc?# & more#]
       (cond
         (string? doc?#) (let [arg# (first more#) body# (rest more#)]
                           (cond
                             (vector? arg#) `(defn ~n# ~doc?# ~(~builder arg# body#))
                             (seq? arg#) (if ~(:overload ameta)
                                           `(defn ~n# ~doc?# ~(map ~builder more#))
                                           `(throw (Exception. "Unsupport overloads.")))
                             :else (throw (Exception. "the arglist must be a vector."))))
         (vector? doc?#) `(defn ~n# ~(~builder doc?# more#))
         (seq? doc?#) (if ~(:overload ameta)
                        `(defn ~n# ~(map ~builder (cons doc?# more#)))
                        `(throw (Exception. "Unsupport overloads.")))
         :else (throw (Exception. "second argument must be a string or vector."))))))

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

(def macro? (check-meta :macro ))

(defmacro auto [ans prefix transform]
  `(do ~@(map (fn [[s#]]
                (let [n# (symbol (clojure.string/replace (str prefix s#) "/" "|"))]
                  `(def ~n# (~transform ~s#))))
           (filter (fn [[_ v#]]
                     (and (fn? @v#) (not (macro? v#)))) (ns-publics ans)))))