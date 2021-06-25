(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/termination/indirectIntro.7.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_42240[24:0]| ( Int) Bool)
(declare-fun |f[8:2][11:0][17:1]| ( Bool  Int  Bool  Int) Bool)
(declare-fun |f[8:1]| ( Bool  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_42240[24:0]| x0))))
(assert (forall ((x0 Int)(var10 Int)(var9 Bool)(x1 Int)) (=> (and (|f[8:1]| var9 var10) (and (|f[8:2][11:0][17:1]| var9 var10 true x1) (<= var10 0))) (|fail_42240[24:0]| x0))))
(assert (forall ((x3 Bool)(x4 Int)(x1 Bool)(x2 Int)) (=> (and (|f[8:1]| x3 x4) (and (not x3) (not x1))) (|f[8:2][11:0][17:1]| x3 x4 x1 x2))))
(assert (forall ((x0 Bool)(x2 Int)) (=> (not x0) (|f[8:1]| x0 x2))))
(check-sat)
(get-model)
(exit)