(ns coreweb.request
  (:use coreweb.safe))

(defn read-request-string [req]
  (let [req (assoc req :params
              (translate-all
                (fn translate-html [request-value]
                  (if (vector? request-value)
                    (mapv translate-html (remove #{""} request-value))
                    (let [param (read-string request-value)]
                      (if (seq? param)
                        request-value
                        (eval param)))))
                (:params req)))]
    req))