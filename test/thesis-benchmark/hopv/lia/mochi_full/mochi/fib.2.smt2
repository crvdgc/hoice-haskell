(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/fib.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_12[0:0]| ( Int) Bool)
(declare-fun |fib[0:1][0:0]| ( Int  Int) Bool)
(declare-fun |fib[0:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_12[0:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (|fib[0:1][0:0]| x1 x2) (>= x1 (+ 1 x2))) (|fail_12[0:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(var39 Int)(var37 Int)(var36 Int)(var38 Int)) (=> (and (|fib[0:1][0:0]| var36 var37) (and (|fib[0:1][0:0]| var38 var39) (and (|fib[0:0]| x0) (and (= x1 (+ var39 var37)) (and (= (+ 2 var36) x0) (and (= (+ 1 var38) x0) (>= x0 2))))))) (|fib[0:1][0:0]| x0 x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|fib[0:0]| x1) (and (= x0 1) (<= x1 1))) (|fib[0:1][0:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)(var40 Int)(x2 Int)) (=> (and (|fib[0:0]| x1) (and (|fib[0:1][0:0]| var40 x2) (and (= (+ 2 x0) x1) (and (>= x1 2) (= (+ 1 var40) x1))))) (|fib[0:0]| x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|fib[0:0]| x1) (and (= x0 1) (<= x1 1))) (|fib[0:1][0:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|fib[0:0]| x1) (and (= (+ 1 x0) x1) (>= x1 2))) (|fib[0:0]| x0))))
(assert (forall ((x0 Int)) (|fib[0:0]| x0)))
(check-sat)
(get-model)
(exit)
