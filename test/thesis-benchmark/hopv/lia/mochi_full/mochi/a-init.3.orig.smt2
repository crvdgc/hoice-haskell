(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/a-init.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_28[180:0]| ( Int) Bool)
(declare-fun |init[143:3][151:0][158:1][173:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |init[146:3][149:0][160:1][171:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |init[146:2][162:1][169:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |init[146:2][162:0]| ( Int  Int  Int) Bool)
(declare-fun |init[146:3][149:0][160:0]| ( Int  Int  Int) Bool)
(declare-fun |init[143:3][151:0][158:0]| ( Int  Int  Int) Bool)
(declare-fun |init[146:1]| ( Int  Int) Bool)
(declare-fun |init[143:1]| ( Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_28[180:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x3 Int)(x4 Int)) (=> (and (|init[143:3][151:0][158:1][173:0]| 0 x3 x1 x4) (and (<= (+ 1 x1) x3) (and (<= 0 x1) (<= x4 0)))) (|fail_28[180:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)(x3 Int)(x4 Int)(x1 Int)) (=> (and (|init[143:1]| x0 x2) (and (|init[146:3][149:0][160:1][171:0]| x1 x2 x3 x4) (and (= x1 (+ 1 x0)) (<= (+ 1 x0) x2)))) (|init[143:3][151:0][158:1][173:0]| x0 x2 x3 x4))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)(x3 Int)) (=> (and (|init[146:1]| x0 x1) (and (|init[146:2][162:1][169:0]| x0 x1 x2 x3) (>= x0 x1))) (|init[146:3][149:0][160:1][171:0]| x0 x1 x2 x3))))
(assert (forall ((x3 Int)(x4 Int)(x1 Int)(x2 Int)(var138 Int)(var137 Int)) (=> (and (|init[143:1]| x1 x4) (and (|init[146:1]| x3 x4) (and (|init[143:1]| x1 var138) (and (|init[146:2][162:0]| var137 var138 x1) (and (<= (+ 1 x1) var138) (and (= var137 (+ 1 x1)) (and (= x2 1) (and (= x3 (+ 1 x1)) (<= (+ 1 x1) x4))))))))) (|init[146:2][162:1][169:0]| x3 x4 x1 x2))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (|init[146:1]| x0 x1) (and (|init[146:3][149:0][160:0]| x0 x1 x2) (>= x0 x1))) (|init[146:2][162:0]| x0 x1 x2))))
(assert (forall ((x2 Int)(x3 Int)(x1 Int)(x0 Int)) (=> (and (|init[143:1]| x0 x3) (and (|init[143:3][151:0][158:0]| x0 x3 x1) (and (|init[146:1]| x2 x3) (and (<= (+ 1 x0) x3) (= x2 (+ 1 x0)))))) (|init[146:3][149:0][160:0]| x2 x3 x1))))
(assert (forall ((x2 Int)(x3 Int)(x1 Int)) (=> (and (|init[143:1]| x2 x3) (and (= x2 0) (and (<= 0 x1) (<= (+ 1 x1) x3)))) (|init[143:3][151:0][158:0]| x2 x3 x1))))
(assert (forall ((x0 Int)(x2 Int)(x1 Int)) (=> (and (|init[143:1]| x1 x2) (and (= x0 (+ 1 x1)) (<= (+ 1 x1) x2))) (|init[146:1]| x0 x2))))
(assert (forall ((x0 Int)(x1 Int)) (=> (= x0 0) (|init[143:1]| x0 x1))))
(check-sat)
(get-model)
(exit)