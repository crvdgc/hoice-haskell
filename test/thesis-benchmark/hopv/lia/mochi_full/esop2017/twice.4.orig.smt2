(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/esop2017/twice.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_22[210:0]| ( Int) Bool)
(declare-fun |mult[188:2][205:0]| ( Int  Int  Int) Bool)
(declare-fun |mult[194:2][197:0]| ( Int  Int  Int) Bool)
(declare-fun |mult[194:1]| ( Int  Int) Bool)
(declare-fun |mult[188:1]| ( Int  Int) Bool)
(declare-fun |mult[167:2][184:0]| ( Int  Int  Int) Bool)
(declare-fun |mult[173:2][176:0]| ( Int  Int  Int) Bool)
(declare-fun |mult[173:1]| ( Int  Int) Bool)
(declare-fun |mult[167:1]| ( Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_22[210:0]| x0))))
(assert (forall ((x0 Int)(x4 Int)(x2 Int)(x1 Int)(x3 Int)) (=> (and (|mult[188:2][205:0]| x2 x3 x4) (and (|mult[167:2][184:0]| x2 x1 x3) (and (>= 0 (+ 1 x4)) (and (<= (+ 1 x2) 0) (>= x1 1))))) (|fail_22[210:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)(var133 Int)(var134 Int)) (=> (and (|mult[194:2][197:0]| x1 var133 var134) (and (|mult[188:1]| x1 x2) (and (= var133 (+ 1 x2)) (and (= (+ x3 x1) var134) (<= (+ 1 x2) 0))))) (|mult[188:2][205:0]| x1 x2 x3))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|mult[194:1]| x1 x2) (and (= x0 0) (= x2 0))) (|mult[194:2][197:0]| x1 x2 x0))))
(assert (forall ((x1 Int)(x0 Int)(x2 Int)) (=> (and (|mult[188:1]| x1 x2) (and (= x0 (+ 1 x2)) (<= (+ 1 x2) 0))) (|mult[194:1]| x1 x0))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|mult[167:2][184:0]| x1 x0 x2) (and (<= (+ 1 x1) 0) (>= x0 1))) (|mult[188:1]| x1 x2))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)(var135 Int)(var136 Int)) (=> (and (|mult[173:2][176:0]| x1 var135 var136) (and (|mult[167:1]| x1 x2) (and (= (+ 1 var135) x2) (and (= x3 (+ x1 var136)) (>= x2 1))))) (|mult[167:2][184:0]| x1 x2 x3))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|mult[173:1]| x1 x2) (and (= x0 0) (= x2 0))) (|mult[173:2][176:0]| x1 x2 x0))))
(assert (forall ((x1 Int)(x0 Int)(x2 Int)) (=> (and (|mult[167:1]| x1 x2) (and (= (+ 1 x0) x2) (>= x2 1))) (|mult[173:1]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (<= (+ 1 x0) 0) (>= x1 1)) (|mult[167:1]| x0 x1))))
(check-sat)
(get-model)
(exit)
