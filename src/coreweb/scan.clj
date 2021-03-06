(ns coreweb.scan
  (:gen-class )
  (:use coreweb.mapping
        coreweb.handler
        coreweb.special
        coreweb.request
        coreweb.tag
        coreweb.safe
        coreweb.template
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

(defn- add-eval-mapping [uri post-uri s nargs i]
  (add-mapping post-uri :post
    (fn [req]
      ((eval `(coreweb.handler/<-
                #(body {} "from:" ~(safe-html (uri-decode uri))
                   (br) ~(str s \: nargs) ~(safe-all (:params req)) \= %)
                ~(str i) ~s)) req))))

(defn- build-post-form [post-uri nargs]
  (apply form {"action" post-uri "method" "post"}
    (let [in #(div {} % \:
                (input {"type" "text" "name" (str %)}))
          has-more (atom false)]
      (concat (doall
                (for [j (range (count nargs))
                      :let [arg (nargs j)]]
                  (cond
                    (= '& arg) (do (reset! has-more true) "")
                    @has-more (let [more (in arg) function (gensym "f")]
                                (reset! has-more function)
                                (str (script {"type" "text/javascript"}
                                       (str "function " function "(node){"
                                         "var div = document.createElement('div');"
                                         "div.innerHTML = '" more "';"
                                         "var element = div.childNodes[0];"
                                         "node.parentNode.insertBefore(element,node);"
                                         "}")) more more))
                    :else (in arg))))
        [(if @has-more (button {"type" "button"
                                "onclick" (str @has-more "(this);")}
                         "more") "") (input {"type" "submit"})]))))

(defn scan-ns [a-ns]
  (try
    (require a-ns)
    (let [path-str (str "/" a-ns "/")]
      (doseq [[s v] (ns-publics a-ns)]
        (let [base (str path-str (uri-encode (name s))) m (meta v)]
          (add-mapping
            (str base ".html")
            :all (<-str
                   {uri :uri}
                   (apply body {} (reduce safe m #{:inline :ns :inline-arities })
                     (let [args (:arglists m)]
                       (for [i (range (count args))]
                         (let [post-uri (str base "@" i ".html") nargs (nth args i)]
                           (add-eval-mapping uri post-uri s nargs i)
                           (build-post-form post-uri nargs))))))))))
    (catch Exception e (.printStackTrace e))))

(defn- mapping-name [file-path]
  (if (.endsWith file-path ".csp")
    (subs file-path 0 (- (count file-path) 4))
    file-path))

(defn- parsing-in-file-system [root-dir]
  (let [root-path (.getPath root-dir)
        r clojure.string/replace
        urif #(r (r (mapping-name (.getPath %)) root-path "") java.io.File/separator "/")]
    (doseq [afile (file-seq root-dir)]
      (when (.isFile afile)
        (if (.endsWith (.getName afile) ".csp")
          (add-mapping (urif afile)
            (<- identity {} (eval (fill afile))))
          (add-mapping (urif afile)
            (<- identity {} afile)))))))

(defn- zip-file [zis dir-str]
  (loop [result {}]
    (if-let [ze (.getNextEntry zis)]
      (let [zn (.getName ze)]
        (cond
          (not (.startsWith zn dir-str)) (recur result)
          (.endsWith zn ".csp") (recur (conj result [zn (fill zis)]))
          (.isDirectory ze) (recur result)
          :else (recur (conj result [zn (let [out (java.io.ByteArrayOutputStream.)]
                                          (copy zis out)
                                          (fn [_] (java.io.ByteArrayInputStream. (.toByteArray out))))]))))
      result)))

(defn- mapping-entry [root-path entry]
  (let [path (clojure.string/replace (entry 0) root-path "")]
    (if (.endsWith path ".csp")
      (add-mapping (mapping-name path) (<- identity {} (eval (entry 1))))
      (add-mapping (mapping-name path) (<- identity {} (entry 1))))))

(defn scan-root [root-path]
  (if-let [root-dir (file (resource root-path))]
    (parsing-in-file-system root-dir)
    (when-let [jar (this-jar coreweb.scan)]
      (let [zip (java.util.zip.ZipInputStream. (.openStream jar))]
        (dorun (map (partial mapping-entry root-path) (zip-file zip root-path)))
        (.close zip)))))