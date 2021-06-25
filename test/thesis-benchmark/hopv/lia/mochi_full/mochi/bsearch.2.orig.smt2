(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/bsearch.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_19[101:0]| ( Int) Bool)
(declare-fun |bs_aux[93:3]| ( Int  Int  Int  Int) Bool)
(declare-fun |bs_aux[74:3]| ( Int  Int  Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_19[101:0]| x0))))
(assert (forall ((x0 Int)(var100 Int)(var99 Int)(x2 Int)(var98 Int)) (=> (and (|bs_aux[93:3]| var98 x2 var99 var100) (and (>= var100 var99) (or (>= 0 (+ 1 (+ var99 (div (- var100 var99) 2)))) (>= (+ var99 (div (- var100 var99) 2)) x2)))) (|fail_19[101:0]| x0))))
(assert (forall ((x1 Int)(x4 Int)(x0 Int)(x3 Int)(x2 Int)(var102 Int)(var103 Int)(var101 Int)) (=> (and (|bs_aux[74:3]| x1 x4 x2 x3) (and (|bs_aux[74:3]| var101 x4 var102 var103) (and (>= x3 x2) (and (= (+ var102 (div (- var103 var102) 2)) (+ x2 (div (- x3 x2) 2))) (and (= x0 (+ (+ x2 (div (- x3 x2) 2)) 1)) (and (>= var103 var102) (and (<= 0 (+ var102 (div (- var103 var102) 2))) (and (<= (+ 1 (+ var102 (div (- var103 var102) 2))) x4) (<= 1 x1))))))))) (|bs_aux[93:3]| x1 x4 x0 x3))))
(assert (forall ((x3 Int)(x4 Int)(x0 Int)(x1 Int)) (=> (and (= x0 0) (and (= (+ 1 x1) x4) (and (<= 0 x3) (= x3 x4)))) (|bs_aux[74:3]| x3 x4 x0 x1))))
(check-sat)
(get-model)
(exit)
