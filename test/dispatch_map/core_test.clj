(ns dispatch-map.core-test
  (:use clojure.test
        dispatch-map.core))

(deftest basic-dispatch-test
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

(deftest isa-dispatch-test
  (testing "dispatch on isa"
    ;; Example from the multimethod docs.
    (derive java.util.Map ::collection)
    (derive java.util.Collection ::collection)
    (let [m (dispatch-map class ::collection :a-collection
                                String :a-string)]
      (is (= :a-collection (m [])))
      (is (= :a-collection (m (java.util.HashMap.))))
      (is (= :a-string (m "bar"))))))
