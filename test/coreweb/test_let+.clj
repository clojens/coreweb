(ns coreweb.test_let+
  (:use clojure.test
        coreweb.let+))

(deftest request-destructuring
  (defn foo [x y]
    (is (= x "bar"))
    (is (= y "baz"))
    nil)
  (testing "vector arguments"
    (let-request++ [[x y] {:params {:x "bar", :y "baz"}}]
      foo))

  (defn xym [x y more]
    (is (= x "foo"))
    (is (= y "bar"))
    (is (= more {:z "baz"}))
    nil)
  (testing "vector '& more' arguments"
    (let-request++ [[x y & more] {:params {:x "foo" :y "bar", :z "baz"}}]
      xym))

  (defn xym2 [x y more]
    (is (= x "foo"))
    (is (= y "bar"))
    (is (= more {"z" "baz"}))
    nil)
  (testing "string parameter names"
    (let-request++ [[x y & more] {:params {"x" "foo" "y" "bar", "z" "baz"}}]
      xym2))

  (defn xr [x r]
    (is (= x "foo"))
    (is (= r {:uri "/foo" :params {:x "foo"}}))
    nil)
  (testing "vector ':as request' arguments"
    (let []
      (let-request++ [[x :as r] {:uri "/foo" :params {:x "foo"}}]
        xr)))

  (defn xyu [x y uri]
    (is (= x "a"))
    (is (= y "b"))
    (is (= uri "/foo"))
    nil)
  (testing "map arguments in vector"
    (let-request++ [[x y :as {uri :uri}] {:params {:x "a", :y "b"} :uri "/foo"}]
      xyu))

  (defn xyz [x y z]
    (is (= x "a"))
    (is (= y "b"))
    (is (= z "c"))
    nil)
  (testing "map arguments2"
    (let-request++ [[x y & {z :z}] {:params {:x "a", :y "b", :z "c"}}]
      xyz))

  (defn xy [x y]
    (is (= x "a"))
    (is (= y "b"))
    nil)

  (testing "arglists binding"
    (let-request++ [0 {:params {:x "a", :y "b", :z "c"}}]
      xy))

  (defn xm [x & more]
    (is (= x "a"))
    (is (= more ["b" "c"])))

  (testing "arglists binding2"
    (let-request++ [0 {:params {:x "a", :y "b", :z "c"}}]
      xy))

  )