(ns dispatch-map.core-test
  (:use clojure.test
        dispatch-map.core))

(deftest basic-dispatch-map-test
  (let [m (dispatch-map identity :a 1 :b 2 :default 0)]
    (testing "basic dispatch"
      (is (= 1 (m :a)))
      (is (= 2 (m :b)))
      (is (= 0 (m :c))))
    (testing "dissoc"
      (let [m (dissoc m :a)]
        (is (= 0 (m :a)))))
    (testing "assoc"
      (let [m (assoc m :c 3)]
        (is (= 3 (m :c)))))))
