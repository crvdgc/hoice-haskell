(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/repeat4.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_11[63:0]| ( Int) Bool)
(declare-fun |repeat[34:2][58:0]| ( Int  Int) Bool)
(declare-fun |repeat[34:0][46:1][52:0]| ( Int  Int) Bool)
(declare-fun |repeat[34:0][46:0]| ( Int) Bool)
(declare-fun |repeat[37:2][40:0]| ( Int  Int) Bool)
(declare-fun |repeat[37:1]| ( Int) Bool)
(declare-fun |repeat[34:1]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_11[63:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)(x1 Int)) (=> (and (|repeat[34:2][58:0]| x1 x2) (not (= x2 x1))) (|fail_11[63:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(var37 Int)(var38 Int)) (=> (and (|repeat[37:2][40:0]| var37 var38) (and (|repeat[34:1]| x0) (and (|repeat[34:0][46:1][52:0]| var38 x1) (and (= (+ 1 var37) x0) (not (= x0 0)))))) (|repeat[34:2][58:0]| x0 x1))))
(assert (forall ((x1 Int)(x2 Int)) (=> (and (|repeat[34:0][46:0]| x1) (= x2 (+ 1 x1))) (|repeat[34:0][46:1][52:0]| x1 x2))))
(assert (forall ((x1 Int)(x0 Int)(var39 Int)) (=> (and (|repeat[34:1]| x0) (and (|repeat[37:2][40:0]| var39 x1) (and (not (= x0 0)) (= (+ 1 var39) x0)))) (|repeat[34:0][46:0]| x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|repeat[37:1]| x1) (and (= x0 0) (= x1 0))) (|repeat[37:2][40:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|repeat[34:1]| x1) (and (= (+ 1 x0) x1) (not (= x1 0)))) (|repeat[37:1]| x0))))
(assert (forall ((x0 Int)) (|repeat[34:1]| x0)))
(check-sat)
(get-model)
(exit)