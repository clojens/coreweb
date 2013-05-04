(ns coreweb.handler
  (:use coreweb.let+
        coreweb.response
        coreweb.special))

(defmacro <- [wrapper bindings & body]
  `(fn [request#]
     (-> (let-request++ [~bindings request#] ~@body)
       ~wrapper
       (render request#))))

(defmacro def-handler-macro [macro-symbol wrapper]
  `(defmacro ~macro-symbol [bindings# & body#]
     `(<- ~'~wrapper ~bindings# ~@body#)))

(def-handler-macro <-str coreweb.special/str-all)