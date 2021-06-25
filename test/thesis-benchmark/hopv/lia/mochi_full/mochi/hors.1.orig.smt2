(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/hors.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_14[18:0]| ( Int) Bool)
(declare-fun |f[12:2]| ( Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_14[18:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(var7 Int)) (=> (and (|f[12:2]| var7 x1) (and (not (= x1 0)) (>= var7 1))) (|fail_14[18:0]| x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (= x0 0) (|f[12:2]| x1 x0))))
(check-sat)
(get-model)
(exit)
