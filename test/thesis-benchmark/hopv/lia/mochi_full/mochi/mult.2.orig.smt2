(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/mult.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_10[51:0]| ( Int) Bool)
(declare-fun |mult[34:2][46:0]| ( Int  Int  Int) Bool)
(declare-fun |mult[37:2][40:0]| ( Int  Int  Int) Bool)
(declare-fun |mult[37:1]| ( Int  Int) Bool)
(declare-fun |mult[34:1]| ( Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_10[51:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (|mult[34:2][46:0]| x1 x1 x2) (>= x1 (+ 1 x2))) (|fail_10[51:0]| x0))))
(assert (forall ((x1 Int)(x0 Int)(x2 Int)(var26 Int)(var25 Int)) (=> (and (|mult[34:1]| x1 x0) (and (|mult[37:2][40:0]| x1 var25 var26) (and (= x2 (+ x1 var26)) (and (>= x1 1) (and (>= x0 1) (= (+ 1 var25) x0)))))) (|mult[34:2][46:0]| x1 x0 x2))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|mult[37:1]| x1 x2) (and (= x0 0) (or (<= x1 0) (<= x2 0)))) (|mult[37:2][40:0]| x1 x2 x0))))
(assert (forall ((x1 Int)(x0 Int)(x2 Int)) (=> (and (|mult[34:1]| x1 x2) (and (= (+ 1 x0) x2) (and (>= x1 1) (>= x2 1)))) (|mult[37:1]| x1 x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (= x1 x0) (|mult[34:1]| x1 x0))))
(check-sat)
(get-model)
(exit)
