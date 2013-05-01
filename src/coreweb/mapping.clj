(ns coreweb.mapping
  (:require clojure.string))

(def method #{:get :post :put :delete :head :options :trace })

(def mapping (atom (sorted-map)))

(defn add-mapping
  ([uri handler]
    (add-mapping uri :all handler))
  ([uri kw handler]
    (swap! mapping assoc-in [uri kw] handler)))

(defn get-handler [uri kw]
  (get-in @mapping [uri kw] (get-in @mapping [uri :all])))

(defn uris [kw]
  (filter (fn [[k v]] (or (contains? v kw) (contains? v :all))) @mapping))