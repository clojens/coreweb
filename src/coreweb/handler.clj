(ns coreweb.handler
  (:use coreweb.str
        coreweb.let+
        coreweb.response))

(defmacro <- [wrap-fn bindings & body]
  `(fn [request#]
     (-> (let-request++ [~bindings request#] ~@body)
       ~wrap-fn
       (render request#))))

(defmacro def-handler-macro [macro-symbol wrap-fn]
  `(defmacro ~macro-symbol [bindings# & body#]
     `(<- ~'~wrap-fn ~bindings# ~@body#)))

(def-handler-macro <-str coreweb.str/str-all)