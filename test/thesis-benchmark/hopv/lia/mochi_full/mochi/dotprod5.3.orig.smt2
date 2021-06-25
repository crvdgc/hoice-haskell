(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/dotprod5.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_25[197:0]| ( Int) Bool)
(declare-fun |bcopy_aux[132:1][190:0]| ( Int  Int) Bool)
(declare-fun |bcopy_aux[179:1][182:0]| ( Int  Int) Bool)
(declare-fun |bcopy_aux[179:3]| ( Int  Int) Bool)
(declare-fun |bcopy_aux[132:2][156:1][171:0]| ( Int  Int  Int) Bool)
(declare-fun |bcopy_aux[132:2][156:0]| ( Int  Int) Bool)
(declare-fun |bcopy_aux[132:1][135:1][150:0]| ( Int  Int  Int) Bool)
(declare-fun |bcopy_aux[132:1][135:0]| ( Int  Int) Bool)
(declare-fun |bcopy_aux[132:3]| ( Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_25[197:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (|bcopy_aux[132:1][190:0]| x2 x1) (or (>= 0 (+ 1 x1)) (>= x1 x2))) (|fail_25[197:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)(var196 Int)(var197 Int)) (=> (and (|bcopy_aux[179:1][182:0]| x1 x2) (and (|bcopy_aux[132:1][135:1][150:0]| x1 x0 var196) (and (|bcopy_aux[132:3]| x1 x0) (and (|bcopy_aux[132:2][156:1][171:0]| x1 x0 var197) (<= (+ 1 x0) x1))))) (|bcopy_aux[132:1][190:0]| x1 x2))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|bcopy_aux[179:3]| x0 x1) (<= (+ 1 x1) x0)) (|bcopy_aux[179:1][182:0]| x0 x1))))
(assert (forall ((x2 Int)(x0 Int)(x1 Int)(x4 Int)(x3 Int)) (=> (and (|bcopy_aux[132:1][135:1][150:0]| x2 x1 x4) (and (|bcopy_aux[132:3]| x2 x1) (and (|bcopy_aux[132:2][156:1][171:0]| x2 x1 x3) (and (= x0 (+ 1 x1)) (<= (+ 1 x1) x2))))) (|bcopy_aux[179:3]| x2 x0))))
(assert (forall ((x0 Int)(x2 Int)(x3 Int)(x1 Int)(var198 Int)) (=> (and (|bcopy_aux[132:2][156:0]| var198 x2) (and (= x3 0) (and (<= 0 x2) (and (<= x0 x1) (and (<= var198 x1) (<= (+ 1 x2) x1)))))) (|bcopy_aux[132:2][156:1][171:0]| x0 x2 x3))))
(assert (forall ((x1 Int)(x2 Int)(var199 Int)) (=> (and (|bcopy_aux[132:3]| x1 x2) (and (|bcopy_aux[132:1][135:1][150:0]| x1 x2 var199) (<= (+ 1 x2) x1))) (|bcopy_aux[132:2][156:0]| x1 x2))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)) (=> (and (|bcopy_aux[132:1][135:0]| x1 x2) (and (= x3 0) (and (<= 0 x2) (<= (+ 1 x2) x1)))) (|bcopy_aux[132:1][135:1][150:0]| x1 x2 x3))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|bcopy_aux[132:3]| x0 x1) (<= (+ 1 x1) x0)) (|bcopy_aux[132:1][135:0]| x0 x1))))
(assert (forall ((x0 Int)(x2 Int)) (=> (= x2 0) (|bcopy_aux[132:3]| x0 x2))))
(check-sat)
(get-model)
(exit)