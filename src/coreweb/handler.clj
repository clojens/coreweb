(ns coreweb.handler
  (:use coreweb.let+
        coreweb.response
        coreweb.special))

(defmacro <- [reuslt-wrapper bindings & body]
  `(fn [request#]
     (-> (let-request++++ [~bindings request#] ~@body)
       (~reuslt-wrapper)
       (render request#))))

(defmacro def-handler-macro [macro-symbol reuslt-wrapper]
  `(defmacro ~macro-symbol [bindings# & body#]
     `(<- ~'~reuslt-wrapper ~bindings# ~@body#)))

(def-handler-macro <-str coreweb.special/str-all)