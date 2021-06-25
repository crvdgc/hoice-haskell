(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/fib.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_12[60:0]| ( Int) Bool)
(declare-fun |fib[34:1][55:0]| ( Int  Int) Bool)
(declare-fun |fib[44:1][47:0]| ( Int  Int) Bool)
(declare-fun |fib[44:0]| ( Int) Bool)
(declare-fun |fib[37:1][40:0]| ( Int  Int) Bool)
(declare-fun |fib[37:0]| ( Int) Bool)
(declare-fun |fib[34:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_12[60:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)) (=> (and (|fib[34:1][55:0]| x1 x2) (>= x1 (+ 1 x2))) (|fail_12[60:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(var39 Int)(var37 Int)(var36 Int)(var38 Int)) (=> (and (|fib[44:1][47:0]| var36 var37) (and (|fib[37:1][40:0]| var38 var39) (and (|fib[34:0]| x0) (and (= x1 (+ var39 var37)) (and (= (+ 2 var36) x0) (and (= (+ 1 var38) x0) (>= x0 2))))))) (|fib[34:1][55:0]| x0 x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|fib[44:0]| x1) (and (= x0 1) (<= x1 1))) (|fib[44:1][47:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)(var40 Int)(x2 Int)) (=> (and (|fib[34:0]| x1) (and (|fib[37:1][40:0]| var40 x2) (and (= (+ 2 x0) x1) (and (>= x1 2) (= (+ 1 var40) x1))))) (|fib[44:0]| x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|fib[37:0]| x1) (and (= x0 1) (<= x1 1))) (|fib[37:1][40:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|fib[34:0]| x1) (and (= (+ 1 x0) x1) (>= x1 2))) (|fib[37:0]| x0))))
(assert (forall ((x0 Int)) (|fib[34:0]| x0)))
(check-sat)
(get-model)
(exit)