(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/fpice/inductive3.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_11[0:0]| ( Int) Bool)
(declare-fun |f[0:1][0:0]| ( Int  Int) Bool)
(declare-fun |f[0:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_11[0:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)) (=> (and (|f[0:1][0:0]| 3 x2) (<= (+ 1 x2) 0)) (|fail_11[0:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)) (=> (and (|f[0:0]| x1) (and (<= x1 1) (and (>= (+ 1 x1) 0) (= (+ 1 x2) (* 2 x1))))) (|f[0:1][0:0]| x1 x2))))
(assert (forall ((x0 Int)) (=> (= x0 3) (|f[0:0]| x0))))
(check-sat)
(get-model)
(exit)
