(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/termination/McCarthy91.5.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_2866[0:0]| ( Int) Bool)
(declare-fun |mc91_without_checking_47[0:3][0:0]| ( Bool  Int  Int  Int) Bool)
(declare-fun |mc91_without_checking_47[0:2]| ( Bool  Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_2866[0:0]| x0))))
(assert (forall ((x0 Int)(x3 Int)(var98 Int)(x1 Int)(var96 Bool)(var97 Int)) (=> (and (|mc91_without_checking_47[0:2]| var96 var97 x3) (and (|mc91_without_checking_47[0:3][0:0]| true x3 var98 x1) (and (<= x3 100) (and (= var98 (+ 11 x3)) (or (<= x1 x3) (<= 112 x1)))))) (|fail_2866[0:0]| x0))))
(assert (forall ((x2 Bool)(x1 Int)(x0 Int)(x3 Int)(var99 Int)(var100 Int)) (=> (and (|mc91_without_checking_47[0:3][0:0]| true x0 var99 var100) (and (|mc91_without_checking_47[0:2]| x2 x1 x0) (and (|mc91_without_checking_47[0:3][0:0]| true x0 var100 x3) (and (= var99 (+ 11 x0)) (and (>= var100 (+ 1 x0)) (and (>= 111 var100) (<= x0 100))))))) (|mc91_without_checking_47[0:3][0:0]| x2 x1 x0 x3))))
(assert (forall ((x1 Bool)(x2 Int)(x3 Int)(x0 Int)) (=> (and (|mc91_without_checking_47[0:2]| x1 x2 x3) (and (= (+ 10 x0) x3) (>= x3 101))) (|mc91_without_checking_47[0:3][0:0]| x1 x2 x3 x0))))
(assert (forall ((x1 Bool)(x2 Int)(x0 Int)(var101 Int)(var102 Bool)(var103 Int)) (=> (and (|mc91_without_checking_47[0:3][0:0]| true x2 var101 x0) (and (|mc91_without_checking_47[0:2]| var102 var103 x2) (and (= var101 (+ 11 x2)) (and (>= x0 (+ 1 x2)) (and (>= 111 x0) (and (<= x2 100) x1)))))) (|mc91_without_checking_47[0:2]| x1 x2 x0))))
(assert (forall ((x1 Bool)(x2 Int)(x3 Int)(x0 Int)) (=> (and (|mc91_without_checking_47[0:2]| x1 x2 x3) (and (= (+ 10 x0) x3) (>= x3 101))) (|mc91_without_checking_47[0:3][0:0]| x1 x2 x3 x0))))
(assert (forall ((x1 Bool)(x4 Int)(x0 Int)(x2 Bool)(x3 Int)) (=> (and (|mc91_without_checking_47[0:2]| x2 x3 x4) (and (= x0 (+ 11 x4)) (and (<= x4 100) x1))) (|mc91_without_checking_47[0:2]| x1 x4 x0))))
(assert (forall ((x1 Bool)(x4 Int)(x0 Int)(x2 Bool)(x3 Int)) (=> (and (|mc91_without_checking_47[0:2]| x2 x3 x4) (and (= x0 (+ 11 x4)) (and (<= x4 100) x1))) (|mc91_without_checking_47[0:2]| x1 x4 x0))))
(assert (forall ((x1 Bool)(x0 Int)(x3 Int)) (=> (and (= x0 0) (not x1)) (|mc91_without_checking_47[0:2]| x1 x0 x3))))
(check-sat)
(get-model)
(exit)