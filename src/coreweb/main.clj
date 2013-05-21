(ns coreweb.main
  (:gen-class )
  (:use ring.adapter.jetty
        ring.middleware.params
        coreweb.mapping
        coreweb.scan
        coreweb.safe)
  (:require clojure.string))

(defn- app [request]
  (let [uri (:uri request)
        request-method (keyword (:request-method request))]
    (if-let [mapping-handle (get-handler (if (= "/" uri) "/index" uri) request-method)]
      (try
        (mapping-handle request)
        (catch Exception e
          (do
            (.printStackTrace e)
            {:status 500
             :headers {"Content-Type" "text/html; charset=UTF-8"}
             :body (str (safe request :body ))})))
      {:status 404
       :headers {"Content-Type" "text/html; charset=UTF-8"}
       :body (str "Not Found:" uri)})))

(defn -main []
  (do
    (scan-prefix-ns 'coreweb.web)
    (scan-ns 'clojure.core)
    (scan-root "coreweb/root")
    (run-jetty (wrap-params app) {:port 3000})))