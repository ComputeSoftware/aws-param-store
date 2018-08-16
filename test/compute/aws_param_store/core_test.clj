(ns compute.aws-param-store.core-test
  (:require
    [clojure.test :refer :all]
    [clojure.spec.test.alpha :as st]
    [compute.aws-param-store.core :as params]))

(st/instrument)

(deftest path*-test
  (is (= "/a" (params/path* [:a])))
  (is (= "/a/b/c" (params/path* [:a/b :c])))
  (is (= "/a" (params/path* ["a"])))
  (is (= "/caa6e6dd-a8b8-4b61-bcb8-26f30bdd4f07"
         (params/path* [#uuid"caa6e6dd-a8b8-4b61-bcb8-26f30bdd4f07"]))))

(deftest parse-path-test
  (is (= ["a"] (params/parse-path "/a")))
  (is (= ["a:b" "c"] (params/parse-path "/a:b/c"))))

(deftest parse-value-test
  (is (= "{asd" (params/parse-value "{asd")))
  (is (= {:a "a"} (params/parse-value (pr-str {:a "a"}))))
  (is (nil? (params/parse-value nil))))

(deftest api-functions-test
  (let [client (params/client {})
        path-vec [:foo :bar]
        val {:a :b}]
    (testing "Can get a parameter after putting"
      (params/put-parameter! client path-vec val)
      (is (= val (params/get-parameter client path-vec))))
    (testing "can get all values at path"
      (is (= {"/foo/bar" val} (params/get-parameters client [:foo]))))
    (testing "Parameter is nil after deleting"
      (params/delete-parameter! client path-vec)
      (is (nil? (params/get-parameter client path-vec))))))