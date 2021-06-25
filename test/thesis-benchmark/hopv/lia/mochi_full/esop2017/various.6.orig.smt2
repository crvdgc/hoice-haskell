(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/esop2017/various.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_36[241:0]| ( Int) Bool)
(declare-fun |mult[233:3][236:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |mult[233:2]| ( Int  Int  Int) Bool)
(declare-fun |mc91[203:2][220:0]| ( Int  Int  Int) Bool)
(declare-fun |mc91[213:2][216:0]| ( Int  Int  Int) Bool)
(declare-fun |mc91[213:1]| ( Int  Int) Bool)
(declare-fun |mc91[206:2][209:0]| ( Int  Int  Int) Bool)
(declare-fun |mc91[206:1]| ( Int  Int) Bool)
(declare-fun |mc91[203:1]| ( Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_36[241:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (|mult[233:3][236:0]| x1 x1 x1 x2) (and (|mc91[203:2][220:0]| x1 x1 91) (and (>= x1 (+ 1 x2)) (<= x1 101)))) (|fail_36[241:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)(x0 Int)) (=> (and (|mult[233:2]| x1 x2 x3) (and (= x0 0) (or (<= x2 0) (<= x3 0)))) (|mult[233:3][236:0]| x1 x2 x3 x0))))
(assert (forall ((x2 Int)(x0 Int)(x1 Int)) (=> (and (|mc91[203:2][220:0]| x2 x2 91) (and (<= x2 101) (and (= x0 x2) (= x0 x1)))) (|mult[233:2]| x2 x0 x1))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)(var394 Int)(var395 Int)) (=> (and (|mc91[213:2][216:0]| x0 var395 x2) (and (|mc91[203:1]| x0 x1) (and (|mc91[206:2][209:0]| x0 var394 var395) (and (<= x1 100) (= var394 (+ 11 x1)))))) (|mc91[203:2][220:0]| x0 x1 x2))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|mc91[213:1]| x1 x2) (and (= (+ 10 x0) x2) (>= x2 101))) (|mc91[213:2][216:0]| x1 x2 x0))))
(assert (forall ((x0 Int)(x2 Int)(x1 Int)(var396 Int)) (=> (and (|mc91[203:1]| x0 x1) (and (|mc91[206:2][209:0]| x0 var396 x2) (and (<= x1 100) (= var396 (+ 11 x1))))) (|mc91[213:1]| x0 x2))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|mc91[206:1]| x1 x2) (and (= (+ 10 x0) x2) (>= x2 101))) (|mc91[206:2][209:0]| x1 x2 x0))))
(assert (forall ((x1 Int)(x0 Int)(x2 Int)) (=> (and (|mc91[203:1]| x1 x2) (and (= x0 (+ 11 x2)) (<= x2 100))) (|mc91[206:1]| x1 x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (<= x1 101) (= x1 x0)) (|mc91[203:1]| x1 x0))))
(check-sat)
(get-model)
(exit)
