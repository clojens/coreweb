(ns coreweb.scan
  (:gen-class )
  (:use coreweb.mapping
        coreweb.handler
        coreweb.special
        clojure.java.io)
  (:require clojure.string))

(defn- domapping [ns-head base-uri]
  (doseq [m (conj method :all )]
    (let [n (symbol (str ns-head "." (name m)))]
      (try
        (require n)
        (doseq [[s v] (ns-publics n)]
          (add-mapping (str base-uri "/" (name s)) m @v))
        (catch Exception e)))))

(defn- file-system-dir [web-dir]
  (loop [cur web-dir remain [] result #{}]
    (cond
      (and (nil? cur) (empty remain)) result
      (nil? cur) (recur (first remain) (rest remain) result)
      :else (recur nil (into remain (filter #(.isDirectory %) (.listFiles cur))) (conj result (.getPath cur))))))

(defn- mapping-in-file-system [ns-head base-file-path file-path]
  (let [r clojure.string/replace
        uri (r (r file-path base-file-path "") java.io.File/separator "/")]
    (domapping ns-head uri)))

(defn- zip-dir [zis dir-str]
  (loop [result #{}]
    (if-let [ze (.getNextEntry zis)]
      (let [zn (.getName ze)]
        (cond
          (not (.startsWith zn dir-str)) (recur result)
          (.isDirectory ze) (recur (conj result (if (.endsWith zn "/") (subs zn 0 (dec (count zn))) zn)))
          (or (.endsWith zn ".clj") (.contains zn "$")) (recur (conj result (subs zn 0 (.lastIndexOf zn "/"))))
          :else (recur result)))
      result)))

(defn- mapping-in-jar [ns-head base-dir dir-name]
  (domapping ns-head (clojure.string/replace dir-name base-dir "")))

(defn- this-jar [& [ns]]
  (-> (or ns (class *ns*)) .getProtectionDomain .getCodeSource .getLocation))

(defn scan-prefix-ns [prefix-ns]
  (let [ns-str (str prefix-ns) dir-str (clojure.string/replace ns-str \. \/)]
    (if-let [web-dir (file (resource dir-str))]
      (dorun (map (partial mapping-in-file-system ns-str (.getPath web-dir)) (file-system-dir web-dir)))
      (when-let [jar (this-jar coreweb.scan)]
        (let [zip (java.util.zip.ZipInputStream. (.openStream jar))]
          (dorun (map (partial mapping-in-jar ns-str dir-str) (zip-dir zip dir-str)))
          (.close zip))))))

(defn scan-ns [a-ns]
  (try
    (require a-ns)
    (let [path-str (str "/" a-ns "/")]
      (doseq [[s v] (ns-publics a-ns)]
        (add-mapping
          (str path-str (uri-encode (name s)) ".html")
          :all (<-str {uri :uri} (meta (resolve (symbol (let [u (uri-decode uri)
                                                              start (count path-str)
                                                              end (.indexOf u ".html")]
                                                          (subs u start end)))))))))
    (catch Exception e)))