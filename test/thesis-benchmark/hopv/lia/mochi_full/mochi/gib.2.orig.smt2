(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/gib.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_23[81:0]| ( Int) Bool)
(declare-fun |gib[68:3][76:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |gib[68:2]| ( Int  Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_23[81:0]| x0))))
(assert (forall ((x0 Int)(x4 Int)(x3 Int)) (=> (and (|gib[68:3][76:0]| 0 1 x3 x4) (<= (+ 1 x4) 0)) (|fail_23[81:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)(x4 Int)) (=> (and (|gib[68:2]| x1 x2 x3) (and (= x3 1) (= x4 x2))) (|gib[68:3][76:0]| x1 x2 x3 x4))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (= x0 0) (= x1 1)) (|gib[68:2]| x0 x1 x2))))
(check-sat)
(get-model)
(exit)