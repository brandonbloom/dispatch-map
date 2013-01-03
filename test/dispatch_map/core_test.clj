(ns dispatch-map.core-test
  (:use [clojure.test]
        [dispatch-map.core]))

;;; Ported multimethod tests

(deftest basic-dispatch-test
  (let [m (dispatch-map identity :a 1 :b 2)]
    (testing "basic dispatch"
      (is (= 1 (m :a)))
      (is (= 1 (get m :a)))
      (is (= 1 (:a m)))
      (is (= [:a 1] (find m :a)))
      (is (= 2 (m :b)))
      (is (= 2 (m :b 0)))
      (is (nil? (m :c)))
      (is (= 0 (m :c 0))))
    (testing "dissoc"
      (let [m (dissoc m :a)]
        (is (nil? (m :a)))))
    (testing "assoc"
      (let [m (assoc m :c 3)]
        (is (= 3 (m :c)))))))

(deftest isa-dispatch-test
  (testing "dispatch on isa"
    (derive java.util.Map ::collection)
    (derive java.util.Collection ::collection)
    (let [m (dispatch-map class ::collection :a-collection
                                String :a-string)]
      (is (= :a-collection (m [])))
      (is (= :a-collection (m (java.util.HashMap.))))
      (is (= :a-string (m "bar"))))))

(deftest preferences-multimethod-test
  (let [m (dispatch-map identity [::rect ::shape] :rect-shape
                                 [::shape ::rect] :shape-rect)]
    (testing "multiple match dispatch error is caught"
      (derive ::rect ::shape)
      (is (thrown? java.lang.IllegalArgumentException
                   (m [::rect ::rect]))))
    (testing "preferences function returns empty table w/ no prefs"
      (= {} (preferences m)))
    (let [m (prefer m [::rect ::shape] [::shape ::rect])]
      (testing "adding a preference to resolve it dispatches correctly"
        (is (= :rect-shape (m [::rect ::rect]))))
      (testing "prefers function now the correct table"
        (is (= {[::rect ::shape] #{[::shape ::rect]}} (preferences m)))))))

;;; dispatch-map specific tests

(deftest map-isms
  (let [m (dispatch-map identity :a 1 :b 2)]
    (testing "vals, keys, and entries"
      (is (= [:a :b] (keys m)))
      (is (= [1 2] (vals m)))
      (is (= [[:a 1] [:b 2]] (seq m))))))
