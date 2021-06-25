(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/sum_cps.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_12[0:0]| ( Int) Bool)
(declare-fun |cps_sum[0:1][0:0]| ( Int  Int) Bool)
(declare-fun |cps_sum[0:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_12[0:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)(x1 Int)) (=> (and (|cps_sum[0:1][0:0]| x1 x2) (<= (+ 1 x2) x1)) (|fail_12[0:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(var35 Int)(var34 Int)) (=> (and (|cps_sum[0:0]| x0) (and (|cps_sum[0:1][0:0]| var34 var35) (and (= x1 (+ var35 x0)) (and (>= x0 1) (= (+ 1 var34) x0))))) (|cps_sum[0:1][0:0]| x0 x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|cps_sum[0:0]| x1) (and (= x0 0) (<= x1 0))) (|cps_sum[0:1][0:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|cps_sum[0:0]| x1) (and (= (+ 1 x0) x1) (>= x1 1))) (|cps_sum[0:0]| x0))))
(assert (forall ((x0 Int)) (|cps_sum[0:0]| x0)))
(check-sat)
(get-model)
(exit)
