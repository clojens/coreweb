(ns coreweb.special
  (:use ring.util.codec))

(defn uri-encode [uri]
  (url-encode (clojure.string/replace uri \? \？))) ;for clojure symbol

(defn uri-decode [uri]
  (clojure.string/replace (url-decode uri) \？ \?)) ;for clojure symbol

(defn str-all [obj]
  (cond
    (map? obj) (str "{" (clojure.string/join ", " (map (fn [[l r]] (str (str-all l) " " (str-all r))) obj)) "}")
    (vector? obj) (str "[" (clojure.string/join ", " (map str-all obj)) "]")
    (set? obj) (str "#{" (clojure.string/join ", " (map str-all obj)) "}")
    (seq? obj) (str "(" (clojure.string/join ", " (map str-all obj)) ")")
    :else (str obj)))

(defn str-coll [obj]
  (if (seq? obj)
    (str-all obj)
    obj))

(defmacro local-bindings []
  (let [symbols ;remove #(.contains (str %) "_")
        (map key @clojure.lang.Compiler/LOCAL_ENV)]
    (zipmap (map (fn [sym] `(quote ~sym)) symbols) symbols)))

(defn schar [i]
  (try
    (char i)
    (catch Exception e nil)))

(defn check-expression
  "opts, :flat not check seq, :pure throw-if not pure in java, :constant check resolve and init"
  [locals obj & opts]
  (let [nil# (gensym "nil_")
        find-one #(or (locals %)
                    (get (locals '&env) % (get locals % nil#)))
        is-final #(java.lang.reflect.Modifier/isFinal (.getModifiers (.field %)))
        ss (if (or (seq? obj) (and (symbol? obj) (.contains (str obj) "/")))
             [(clojure.lang.Compiler/analyze clojure.lang.Compiler$C/EXPRESSION obj)]
             [obj])]
    (loop [[s & more :as all] ss]
      (let [cs (class s)]
        (cond
          (empty? all) true
          (nil? s) (if (some #{:constant })
                     (throw (Exception. "null init"))
                     (recur more))
          (seq? s) (if (some #{:flat } opts)
                     (recur more)
                     (recur (concat s more)))
          (and (symbol? s) (= nil# (find-one s))) (if-let [rs (resolve s)]
                                                    (if (.startsWith (str @rs) "Unbound:")
                                                      (throw (Exception. (str "unbound resolve:" s)))
                                                      (recur more))
                                                    (throw (Exception. (str "null resolve:" s))))
          (symbol? s) (recur (conj more (find-one s)))
          (= clojure.lang.Compiler$BodyExpr cs) (recur (into more (.exprs s)))
          (= clojure.lang.Compiler$CaseExpr cs) (recur (conj (concat (vals (.tests s)) (vals (.thens s)) more)
                                                         (.defaultExpr s) (.expr s)))
          (= clojure.lang.Compiler$FnExpr cs) (recur (into more (.methods s)))
          (= clojure.lang.Compiler$FnMethod cs) (recur
                                                  (let [b (into more (.reqParms s))]
                                                    (if-let [r (.restParm s)]
                                                      (conj b r) b)))
          (= clojure.lang.Compiler$IfExpr cs) (recur (conj more (.testExpr s) (.thenExpr s) (.elseExpr s)))
          (= clojure.lang.Compiler$InstanceFieldExpr cs) (if (and (some #{:pure } opts) (not (is-final s)))
                                                           (throw (Exception. (str "not pure:" s)))
                                                           (recur (conj more (.target s))))
          (= clojure.lang.Compiler$InstanceMethodExpr cs) (if (some #{:pure } opts)
                                                            (throw (Exception. (str "not pure:" s)))
                                                            (recur (conj (into more (.args s)) (.target s))))
          (= clojure.lang.Compiler$InstanceOfExpr cs) (recur (conj more (.expr s)))
          (= clojure.lang.Compiler$InvokeExpr cs) (recur (conj (into more (.args s)) (.fexpr s)))
          (= clojure.lang.Compiler$LocalBindingExpr cs) (recur (conj more (.b s)))
          (= clojure.lang.Compiler$MapExpr cs) (recur (into more (.keyvals s)))
          (= clojure.lang.Compiler$MetaExpr cs) (recur (conj more (.expr s) (.meta s)))
          (= clojure.lang.Compiler$NewInstanceExpr cs) (recur (into more (.methods s)))
          (= clojure.lang.Compiler$NewInstanceMethod cs) (recur (into more (.parms s)))
          (= clojure.lang.Compiler$SetExpr cs) (recur (into more (.keys s)))
          (= clojure.lang.Compiler$StaticFieldExpr cs) (if (and (some #{:pure } opts) (not (is-final s)))
                                                         (throw (Exception. (str "not pure:" s)))
                                                         (recur more))
          (= clojure.lang.Compiler$StaticMethodExpr cs) (if (some #{:pure } opts)
                                                          (throw (Exception. (str "not pure:" s)))
                                                          (recur (into more (.args s))))
          (= clojure.lang.Compiler$ThrowExpr cs) (recur (conj more (.excExpr s)))
          (= clojure.lang.Compiler$TryExpr cs) (recur (conj (into more (.catchExprs s)) (.tryExpr s) (.finallyExpr s)))
          (= clojure.lang.Compiler$UnresolvedVarExpr cs) (recur (conj more (.symbol s)))
          (contains? #{clojure.lang.Compiler$LetExpr
                       clojure.lang.Compiler$LetFnExpr} cs) (recur (conj (into more (.bindingInits s)) (.body s)))
          (contains? #{clojure.lang.Compiler$TheVarExpr
                       clojure.lang.Compiler$VarExpr} cs) (recur (conj more (.deref (.var s))))
          (contains? #{clojure.lang.Compiler$BindingInit
                       clojure.lang.Compiler$DefExpr
                       clojure.lang.Compiler$LocalBinding} cs) (recur (conj more (.init s)))
          (contains? #{clojure.lang.Compiler$KeywordInvokeExpr
                       clojure.lang.Compiler$MonitorEnterExpr
                       clojure.lang.Compiler$MonitorExitExpr} cs) (recur (conj more (.target s)))
          (contains? #{clojure.lang.Compiler$ListExpr
                       clojure.lang.Compiler$NewExpr
                       clojure.lang.Compiler$RecurExpr
                       clojure.lang.Compiler$StaticInvokeExpr
                       clojure.lang.Compiler$VectorExpr} cs) (recur (into more (.args s)))
          (contains? #{clojure.lang.Compiler$AssignExpr ;always nil
                       clojure.lang.Compiler$BooleanExpr
                       clojure.lang.Compiler$ConstantExpr
                       clojure.lang.Compiler$EmptyExpr
                       clojure.lang.Compiler$ImportExpr ;always nil
                       clojure.lang.Compiler$NilExpr
                       clojure.lang.Compiler$NumberExpr
                       clojure.lang.Compiler$KeywordExpr
                       clojure.lang.Compiler$StringExpr} cs) (recur more)
          (.contains (str cs) "clojure.lang.Compiler$") (throw (Exception. (str "not analyze completely:" cs)))
          :else (recur more))))))