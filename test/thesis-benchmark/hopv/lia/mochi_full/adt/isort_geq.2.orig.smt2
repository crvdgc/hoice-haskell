(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/adt/isort_geq.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_72[102:0]| ( Int) Bool)
(declare-fun |length[76:2][97:0]| ( Int  Int) Bool)
(declare-fun |length[84:2][87:0]| ( Int  Int) Bool)
(declare-fun |length[84:0]| ( Int) Bool)
(declare-fun |length[76:0]| ( Int) Bool)
(declare-fun |length[69:2][72:0]| ( Int  Int) Bool)
(declare-fun |length[69:0]| ( Int) Bool)
(declare-fun |insertsort[62:2][65:0]| ( Int  Int) Bool)
(declare-fun |insertsort[62:0]| ( Int) Bool)
(declare-fun |make_list[55:1][58:0]| ( Int  Int) Bool)
(declare-fun |make_list[55:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_72[102:0]| x0))))
(assert (forall ((x0 Int)(x3 Int)(x5 Int)(x4 Int)(x2 Int)(x1 Int)) (=> (and (|length[76:2][97:0]| x4 x5) (and (|length[69:2][72:0]| x2 x3) (and (|insertsort[62:2][65:0]| x4 x2) (and (|make_list[55:1][58:0]| x1 x4) (<= (+ 1 x3) x5))))) (|fail_72[102:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)(var360 Int)(var361 Int)) (=> (and (|length[84:2][87:0]| var360 var361) (and (|length[76:0]| x1) (and (<= 1 x1) (and (= (+ 1 var360) x1) (= x2 (+ 1 var361)))))) (|length[76:2][97:0]| x1 x2))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|length[84:0]| x1) (and (= x0 0) (<= x1 0))) (|length[84:2][87:0]| x1 x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|length[76:0]| x0) (and (= (+ 1 x1) x0) (>= x0 1))) (|length[84:0]| x1))))
(assert (forall ((x2 Int)(x1 Int)(x3 Int)(x0 Int)) (=> (and (|length[69:2][72:0]| x1 x3) (and (|insertsort[62:2][65:0]| x2 x1) (|make_list[55:1][58:0]| x0 x2))) (|length[76:0]| x2))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|length[69:0]| x1) (and (= x0 0) (<= x1 0))) (|length[69:2][72:0]| x1 x0))))
(assert (forall ((x2 Int)(x1 Int)(x0 Int)) (=> (and (|insertsort[62:2][65:0]| x1 x2) (|make_list[55:1][58:0]| x0 x1)) (|length[69:0]| x2))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|insertsort[62:0]| x1) (and (= x0 0) (<= x1 0))) (|insertsort[62:2][65:0]| x1 x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (|make_list[55:1][58:0]| x0 x1) (|insertsort[62:0]| x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|make_list[55:0]| x1) (and (= x0 0) (= x1 0))) (|make_list[55:1][58:0]| x1 x0))))
(assert (forall ((x0 Int)) (|make_list[55:0]| x0)))
(check-sat)
(get-model)
(exit)
