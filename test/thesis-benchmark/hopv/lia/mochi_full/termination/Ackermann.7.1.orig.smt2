(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/termination/Ackermann.7.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_12614[34:0]| ( Int) Bool)
(declare-fun |ack_without_checking_92[20:7]| ( Bool  Int  Int  Int  Bool  Int  Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_12614[34:0]| x0))))
(assert (forall ((x0 Int)(x8 Int)(x4 Int)(var20 Bool)(var21 Int)(var22 Int)(var23 Bool)(var24 Int)(var25 Int)) (=> (and (|ack_without_checking_92[20:7]| var20 var21 var22 x4 var23 var24 var25 x8) (and (<= (+ 1 x8) 0) (not (= x4 0)))) (|fail_12614[34:0]| x0))))
(assert (forall ((x6 Bool)(x4 Int)(x5 Int)(x3 Int)(x2 Bool)(x1 Int)(x0 Int)(x8 Int)) (=> (and (= x4 0) (and (= x5 0) (and (>= x8 1) (and (>= x3 1) (and (= x5 x0) (and (= x4 x1) (and (not x6) (not x2)))))))) (|ack_without_checking_92[20:7]| x6 x4 x5 x3 x2 x1 x0 x8))))
(check-sat)
(get-model)
(exit)
