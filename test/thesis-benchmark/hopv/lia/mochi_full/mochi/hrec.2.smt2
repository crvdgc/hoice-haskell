(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/hrec.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_12[0:0]| ( Int) Bool)
(declare-fun |f[0:2][0:0]| ( Int  Int) Bool)
(declare-fun |f[0:0][0:1][0:0]| ( Int  Int) Bool)
(declare-fun |f[0:0][0:0]| ( Int) Bool)
(declare-fun |f[0:1]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_12[0:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)(x1 Int)) (=> (and (|f[0:2][0:0]| x1 x2) (<= (+ 1 x2) 0)) (|fail_12[0:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(var27 Int)) (=> (and (|f[0:2][0:0]| var27 x1) (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 var27) (<= (+ 1 x0) 0)))) (|f[0:2][0:0]| x0 x1))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 x1) (>= x0 0))) (|f[0:2][0:0]| x0 x1))))
(assert (forall ((x2 Int)(x3 Int)(x0 Int)(x1 Int)) (=> (and (|f[0:2][0:0]| x2 x3) (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 x1) (<= (+ 1 x0) 0)))) (|f[0:0][0:1][0:0]| x2 x3))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 x1) (>= x0 0))) (|f[0:2][0:0]| x0 x1))))
(assert (forall ((x2 Int)(x3 Int)(x1 Int)(x0 Int)) (=> (and (|f[0:0][0:1][0:0]| x1 x0) (and (|f[0:1]| x1) (and (|f[0:0][0:1][0:0]| x2 x3) (<= (+ 1 x1) 0)))) (|f[0:0][0:1][0:0]| x2 x3))))
(assert (forall ((x1 Int)(x2 Int)) (=> (and (|f[0:0][0:0]| x1) (= x2 (+ 1 x1))) (|f[0:0][0:1][0:0]| x1 x2))))
(assert (forall ((x1 Int)(x0 Int)(var28 Int)) (=> (and (|f[0:0][0:0]| x1) (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 var28) (<= (+ 1 x0) 0)))) (|f[0:0][0:0]| x1))))
(assert (forall ((x0 Int)) (=> (and (|f[0:1]| x0) (>= x0 0)) (|f[0:0][0:0]| x0))))
(assert (forall ((x2 Int)(x0 Int)(x1 Int)) (=> (and (|f[0:0][0:0]| x2) (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 x1) (<= (+ 1 x0) 0)))) (|f[0:1]| x2))))
(assert (forall ((x0 Int)) (=> (and (|f[0:1]| x0) (>= x0 0)) (|f[0:0][0:0]| x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|f[0:1]| x0) (and (|f[0:0][0:1][0:0]| x0 x1) (<= (+ 1 x0) 0))) (|f[0:1]| x1))))
(assert (forall ((x1 Int)(x2 Int)) (=> (and (|f[0:0][0:0]| x1) (= x2 (+ 1 x1))) (|f[0:0][0:1][0:0]| x1 x2))))
(assert (forall ((x0 Int)) (=> (and (|f[0:1]| x0) (<= (+ 1 x0) 0)) (|f[0:0][0:0]| x0))))
(assert (forall ((x0 Int)) (|f[0:1]| x0)))
(check-sat)
(get-model)
(exit)
