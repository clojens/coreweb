(ns coreweb.web.get
  (:use coreweb.tag
        coreweb.mapping
        coreweb.handler
        coreweb.special)
  (:require clojure.string))

(def get-urls
  (<-str {uri :uri}
    (str "Hello World:" uri (br)
      (clojure.string/join
        (br)
        (map #(let [[u] %] (a {"href" u} (clojure.string/replace (uri-decode u) ".html" ""))) (uris :get ))))))