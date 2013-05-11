(ns coreweb.test_let+
  (:use clojure.test
        coreweb.let+))

(def n "0")

(def v '[x y])

(deftest request-destructuring
  (defn foo [x y]
    (is (= x "bar"))
    (is (= y "baz"))
    nil)
  (testing "vector arguments"
    (let-request++++ [[x y] {:params {:x "bar", :y "baz"}}]
      foo))

  (defn yx [y x]
    (is (= x "x"))
    (is (= y "y"))
    nil)
  (testing "vector arguments2"
    (let-request++++ [[y x] {:params {:x "x", :y "y"}}]
      yx))

  (defn xym [x y more]
    (is (= x "foo"))
    (is (= y "bar"))
    (is (= more {:z "baz"}))
    nil)
  (testing "vector '& more' arguments"
    (let-request++++ [[x y & more] {:params {:x "foo" :y "bar", :z "baz"}}]
      xym))

  (defn xym2 [x y more]
    (is (= x "foo"))
    (is (= y "bar"))
    (is (= more {"z" "baz"}))
    nil)
  (testing "string parameter names"
    (let-request++++ [[x y & more] {:params {"x" "foo" "y" "bar", "z" "baz"}}]
      xym2))

  (defn xr [x r]
    (is (= x "foo"))
    (is (= r {:uri "/foo" :params {:x "foo"}}))
    nil)
  (testing "vector ':as request' arguments"
    (let []
      (let-request++++ [[x :as r] {:uri "/foo" :params {:x "foo"}}]
        xr)))

  (defn xyu [x y uri]
    (is (= x "a"))
    (is (= y "b"))
    (is (= uri "/foo"))
    nil)
  (testing "map arguments in vector"
    (let-request++++ [[x y :as {uri :uri}] {:params {:x "a", :y "b"} :uri "/foo"}]
      xyu))

  (defn xyz [x y z]
    (is (= x "a"))
    (is (= y "b"))
    (is (= z "c"))
    nil)
  (testing "map arguments2"
    (let-request++++ [[x y & {z :z}] {:params {:x "a", :y "b", :z "c"}}]
      xyz))

  (defn xy [x y]
    (is (= x "a"))
    (is (= y "b"))
    nil)

  (testing "arglists binding"
    (let-request++++ [0 {:params {:x "a", :y "b", :z "c"}}]
      xy))

  (defn xm [x & more]
    (is (= x "a"))
    (is (= more (list "b" "c"))))

  (testing "arglists binding2"
    (let-request++++ [0 {:params {:x "a", :more ["b" "c"]}}]
      xm))

  (defn foo-s [x y]
    (is (= x 1))
    (is (= y 2))
    nil)
  (testing "str vector arguments"
    (let-request++++ ["[x y]" {:params {:x "1", :y "2"}}]
      foo-s))

  (defn yx-s [y x]
    (is (= x 1))
    (is (= y 2))
    nil)
  (testing "str vector arguments2"
    (let-request++++ ["[y x]" {:params {:x "1", :y "2"}}]
      yx-s))

  (defn xym-s [x y more]
    (is (= x 1))
    (is (= y 2))
    (is (= more {:z 3}))
    nil)
  (testing "str vector '& more' arguments"
    (let-request++++ ["[x y & more]" {:params {:x "1" :y "2", :z "3"}}]
      xym-s))

  (defn xym2-s [x y more]
    (is (= x 1))
    (is (= y 2))
    (is (= more {"z" 3}))
    nil)
  (testing "str string parameter names"
    (let-request++++ ["[x y & more]" {:params {"x" "1" "y" "2", "z" "3"}}]
      xym2-s))

  (defn xr-s [x r]
    (is (= x inc))
    (is (= r {:uri "/foo" :params {:x inc}}))
    nil)
  (testing "str vector ':as request' arguments"
    (let []
      (let-request++++ ["[x :as r]" {:uri "/foo" :params {:x "inc"}}]
        xr-s)))

  (defn xyu-s [x y uri]
    (is (= x 1))
    (is (= y 2))
    (is (= uri "/foo"))
    nil)
  (testing "str map arguments in vector"
    (let-request++++ ["[x y :as {uri :uri}]" {:params {:x "1", :y "2"} :uri "/foo"}]
      xyu-s))

  (defn xyz-s [x y z]
    (is (= x 1))
    (is (= y 2))
    (is (= z 3))
    nil)
  (testing "str map arguments2"
    (let-request++++ ["[x y & {z :z}]" {:params {:x "1", :y "2", :z "3"}}]
      xyz-s))

  (defn xy-s [x y]
    (is (= x 1))
    (is (= y 2))
    nil)

  (testing "str arglists binding"
    (let-request++++ ["0" {:params {:x "1", :y "2", :z "3"}}]
      xy-s))

  (defn xm-s [x & more]
    (is (= x 1))
    (is (= more (list 2 3))))

  (testing "str arglists binding2"
    (let-request++++ ["0" {:params {:x "1", :more ["2" "3"]}}]
      xm-s))

  (testing "symbol binding"
    (let-request++++ [n {:params {:x "1", :more ["2" "3"]}}]
      xm-s))

  (testing "symbol binding2"
    (let [ttt 0]
      (let-request++++ [ttt {:params {:x "a", :more ["b" "c"]}}]
        xm)))

  (testing "symbol binding3"
    (let [ttt n]
      (let-request++++ [ttt {:params {:x "1", :more ["2" "3"]}}]
        xm-s)))

  (def yv "2")

  (testing "symbol binding4"
    (let [ttt v]
      (let-request++++ [ttt {:params {:x "1", :y "2"}}]
        (is (= x "1"))
        (is (= y yv)))))
  )