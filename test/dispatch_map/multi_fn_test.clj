(ns dispatch-map.multi-fn-test
  (:refer-clojure :exclude [defmulti defmethod remove-all-methods remove-method
                            prefer-method methods get-method prefers])
  (:use [clojure.test]
        [dispatch-map.multi-fn]))

;;; Tests adapted from Clojure's own tests.
;;; Copyright (c) Rich Hickey.
;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)

(deftest basic-multimethod-test
  (testing "Check basic dispatch"
    (defmulti too-simple identity)
    (defmethod too-simple :a [x] :a)
    (defmethod too-simple :b [x] :b)
    (defmethod too-simple :default [x] :default)
    (is (= :a (too-simple :a)))
    (is (= :b (too-simple :b)))
    (is (= :default (too-simple :c))))
  (testing "Remove a method works"
    (remove-method too-simple :a)
    (is (= :default (too-simple :a))))
  (testing "Add another method works"
    (defmethod too-simple :d [x] :d)
    (is (= :d (too-simple :d)))))

(deftest isA-multimethod-test
  (testing "Dispatch on isA"
    ;; Example from the multimethod docs.
    (derive java.util.Map ::collection)
    (derive java.util.Collection ::collection)
    (defmulti foo class)
    (defmethod foo ::collection [c] :a-collection)
    (defmethod foo String [s] :a-string)
    (is (= :a-collection (foo [])))
    (is (= :a-collection (foo (java.util.HashMap.))))
    (is (= :a-string (foo "bar")))))

(deftest preferences-multimethod-test
 (testing "Multiple method match dispatch error is caught"
    ;; Example from the multimethod docs.
    (derive ::rect ::shape)
    (defmulti bar (fn [x y] [x y]))
    (defmethod bar [::rect ::shape] [x y] :rect-shape)
    (defmethod bar [::shape ::rect] [x y] :shape-rect)
    (is (thrown? java.lang.IllegalArgumentException
                 (bar ::rect ::rect))))
 (testing "The prefers method returns empty table w/ no prefs"
   (= {} (prefers bar)))
 (testing "Adding a preference to resolve it dispatches correctly"
   (prefer-method bar [::rect ::shape] [::shape ::rect])
   (is (= :rect-shape (bar ::rect ::rect))))
 (testing "The prefers method now returns the correct table"
   (is (= {[::rect ::shape] #{[::shape ::rect]}} (prefers bar)))))

(deftest remove-all-methods-test
  (testing "Core function remove-all-methods works"
    (defmulti simple identity)
    (defmethod simple :a [x] :a)
    (defmethod simple :b [x] :b)
    (is (= {} (methods (remove-all-methods simple))))))

(deftest methods-test
  (testing "Core function methods works"
    (defmulti simple identity)
    (defmethod simple :a [x] :a)
    (defmethod simple :b [x] :b)
    (is (= #{:a :b} (into #{} (keys (methods simple)))))
    (is (= :a ((:a (methods simple)) 1)))
    (defmethod simple :c [x] :c)
    (is (= #{:a :b :c} (into #{} (keys (methods simple)))))
    (remove-method simple :a)
    (is (= #{:b :c} (into #{} (keys (methods simple)))))))

(deftest get-method-test
  (testing "Core function get-method works"
    (defmulti simple identity)
    (defmethod simple :a [x] :a)
    (defmethod simple :b [x] :b)
    (is (fn? (get-method simple :a)))
    (= (:a ((get-method simple :a) 1)))
    (is (fn? (get-method simple :b)))
    (= (:b ((get-method simple :b) 1)))
    (is (nil? (get-method simple :c)))))
