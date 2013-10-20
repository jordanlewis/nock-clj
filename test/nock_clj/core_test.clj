(ns nock-clj.core-test
  (:require [clojure.test :refer :all]
            [nock-clj.core :refer :all]))

(deftest slot-test
  (testing "slot examples")
    (is (= '((4 5) (6 14 15))   (slot '(1 (4 5) (6 14 15)))))
    (is (= '(4 5)               (slot '(2 (4 5) (6 14 15)))))
    (is (= '(6 14 15)           (slot '(3 (4 5) (6 14 15)))))
    (is (= 4                    (slot '(4 (4 5) (6 14 15)))))
    (is (= 5                    (slot '(5 (4 5) (6 14 15)))))
    (is (= 6                    (slot '(6 (4 5) (6 14 15)))))
    (is (= '(14 15)             (slot '(7 (4 5) (6 14 15))))))

(deftest tar-test
  (testing "tar examples")
    (is (= '(14 15) (tar '(((4 5) (6 14 15)) 0 7))))
    (is (= '(153 218) (tar '(42 1 153 218))))
    (is (= '(153 218) (tar (eat [77 [2 [1 42] [1 1 153 218]]]))))

    (is (= 57 (tar '(57 0 1))))
    (is (= 19 (tar '((132 19) 0 3))))
    (is (= 58 (tar '(57 4 0 1))))
    (is (= 20 (tar '((132 19) 4 0 3))))
    (is (= '(43 1) (tar (eat [42 [[4 0 1] [3 0 1]]]))))
    (is (= 20 (tar (eat [[132 19] [10 37 [4 0 3]]]))))
    (is (= 44 (tar (eat [42 [7 [4 0 1] [4 0 1]]]))))
    (is (= '(43 42) (tar (eat [42 [8 [4 0 1] [0 1]]]))))
    (is (= 43 (tar (eat [42 [8 [4 0 1] [4 0 3]]]))))
    (is (= 43 (tar (eat [42 [6 [1 0] [4 0 1] [1 233]]]))))
    (is (= 233 (tar (eat [42 [6 [1 1] [4 0 1] [1 233]]]))))
  )

(deftest dec-test
  (testing "Decrement 42")
    (is (= 41 (tar (eat [42 [8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]])))))

(run-tests)
