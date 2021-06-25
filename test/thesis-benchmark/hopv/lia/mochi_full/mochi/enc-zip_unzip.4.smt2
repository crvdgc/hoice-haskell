(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/mochi/enc-zip_unzip.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_21[0:0]| ( Int) Bool)
(declare-fun |zip[0:1]| ( Int  Int) Bool)
(declare-fun |unzip[0:1][0:1]| ( Int  Int  Int) Bool)
(declare-fun |unzip[0:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_21[0:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)) (=> (and (|zip[0:1]| 0 x2) (not (= x2 0))) (|fail_21[0:0]| x0))))
(assert (forall ((x1 Int)(x0 Int)(x3 Int)(x2 Int)) (=> (and (|zip[0:1]| x2 x3) (and (= (+ 1 x0) x3) (and (= (+ 1 x1) x2) (and (not (= x2 0)) (not (= x3 0)))))) (|zip[0:1]| x1 x0))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (|unzip[0:1][0:1]| x0 x1 x2) (|zip[0:1]| x1 x2))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)(var203 Int)(var204 Int)(var205 Int)(var206 Int)) (=> (and (|unzip[0:0]| x0) (and (|unzip[0:0]| var203) (and (|unzip[0:1][0:1]| var204 var205 var206) (and (not (= var203 0)) (and (= (+ 1 var204) var203) (and (= x1 (+ 1 var205)) (and (= x2 (+ 1 var206)) (not (= x0 0))))))))) (|unzip[0:1][0:1]| x0 x1 x2))))
(assert (forall ((x2 Int)(x0 Int)(x1 Int)) (=> (and (|unzip[0:0]| x2) (and (= x0 0) (and (= x1 0) (= x2 0)))) (|unzip[0:1][0:1]| x2 x0 x1))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|unzip[0:0]| x1) (and (= (+ 1 x0) x1) (not (= x1 0)))) (|unzip[0:0]| x0))))
(assert (forall ((x0 Int)) (|unzip[0:0]| x0)))
(check-sat)
(get-model)
(exit)