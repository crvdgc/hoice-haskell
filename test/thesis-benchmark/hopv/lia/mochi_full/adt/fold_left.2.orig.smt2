(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/adt/fold_left.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_28[146:0]| ( Int) Bool)
(declare-fun |fold_left[71:4][141:0]| ( Int  Int  Int) Bool)
(declare-fun |fold_left[128:4][131:0]| ( Int  Int  Int) Bool)
(declare-fun |fold_left[128:2]| ( Int  Int) Bool)
(declare-fun |fold_left[71:0][112:2][118:0]| ( Int  Int  Int) Bool)
(declare-fun |fold_left[71:0][112:1]| ( Int  Int) Bool)
(declare-fun |fold_left[71:3][79:1][98:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |make_list[55:1][67:1][83:1][94:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |make_list[55:1][67:1][83:0]| ( Int  Int  Int) Bool)
(declare-fun |fold_left[71:3][79:0]| ( Int  Int  Int) Bool)
(declare-fun |fold_left[71:2]| ( Int  Int) Bool)
(declare-fun |make_list[55:1][67:0]| ( Int  Int) Bool)
(declare-fun |make_list[58:1][61:0]| ( Int  Int) Bool)
(declare-fun |make_list[58:0]| ( Int) Bool)
(declare-fun |make_list[55:0]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_28[146:0]| x0))))
(assert (forall ((x0 Int)(x4 Int)(x2 Int)(x3 Int)(x1 Int)) (=> (and (|fold_left[71:4][141:0]| x2 x3 x4) (and (|make_list[55:1][67:0]| x1 x3) (<= (+ 1 x4) x2))) (|fail_28[146:0]| x0))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)(var167 Int)(var169 Int)(var168 Int)) (=> (and (|fold_left[128:4][131:0]| var169 var167 x3) (and (|fold_left[71:3][79:1][98:0]| x1 x2 0 var168) (and (|fold_left[71:2]| x1 x2) (and (|fold_left[71:0][112:2][118:0]| x1 var168 var169) (and (<= 1 x2) (= (+ 1 var167) x2)))))) (|fold_left[71:4][141:0]| x1 x2 x3))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (and (|fold_left[128:2]| x1 x2) (and (<= x2 0) (= x0 x1))) (|fold_left[128:4][131:0]| x1 x2 x0))))
(assert (forall ((x4 Int)(x2 Int)(x3 Int)(x0 Int)(x1 Int)) (=> (and (|fold_left[71:3][79:1][98:0]| x0 x3 0 x1) (and (|fold_left[71:2]| x0 x3) (and (|fold_left[71:0][112:2][118:0]| x0 x1 x4) (and (= (+ 1 x2) x3) (>= x3 1))))) (|fold_left[128:2]| x4 x2))))
(assert (forall ((x3 Int)(x4 Int)(x5 Int)(x0 Int)(x2 Int)) (=> (and (|make_list[55:1][67:0]| x0 x2) (and (|fold_left[71:0][112:1]| x3 x4) (= x5 (+ x3 x4)))) (|fold_left[71:0][112:2][118:0]| x3 x4 x5))))
(assert (forall ((x3 Int)(x4 Int)(var172 Int)) (=> (and (|fold_left[71:3][79:1][98:0]| x3 var172 0 x4) (and (|fold_left[71:2]| x3 var172) (<= 1 var172))) (|fold_left[71:0][112:1]| x3 x4))))
(assert (forall ((x3 Int)(x4 Int)(x1 Int)(x2 Int)(x0 Int)) (=> (and (|fold_left[71:2]| x3 x4) (and (|make_list[55:1][67:1][83:1][94:0]| x0 x4 x1 x2) (|make_list[55:1][67:0]| x0 x4))) (|fold_left[71:3][79:1][98:0]| x3 x4 x1 x2))))
(assert (forall ((x2 Int)(x3 Int)(x0 Int)(x1 Int)(var175 Int)(var177 Int)(var176 Int)) (=> (and (|make_list[55:1][67:1][83:0]| x2 var175 x0) (and (|make_list[58:1][61:0]| var176 var177) (and (|make_list[55:1][67:0]| x2 x3) (and (|make_list[55:0]| x2) (and (= var175 (+ 1 var177)) (and (= x3 (+ 1 var177)) (and (= (+ 1 var176) x2) (and (= x0 0) (and (>= x2 0) (= x1 x2)))))))))) (|make_list[55:1][67:1][83:1][94:0]| x2 x3 x0 x1))))
(assert (forall ((x3 Int)(x1 Int)(x2 Int)(x0 Int)) (=> (and (|make_list[55:0]| x3) (and (|fold_left[71:3][79:0]| x0 x1 x2) (|make_list[55:1][67:0]| x3 x1))) (|make_list[55:1][67:1][83:0]| x3 x1 x2))))
(assert (forall ((x1 Int)(x2 Int)(x3 Int)) (=> (and (|fold_left[71:2]| x1 x2) (and (= x3 0) (<= 1 x2))) (|fold_left[71:3][79:0]| x1 x2 x3))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)) (=> (|make_list[55:1][67:0]| x0 x2) (|fold_left[71:2]| x1 x2))))
(assert (forall ((x0 Int)(x1 Int)(var179 Int)(var178 Int)) (=> (and (|make_list[55:0]| x0) (and (|make_list[58:1][61:0]| var178 var179) (and (= x1 (+ 1 var179)) (and (>= x0 0) (= (+ 1 var178) x0))))) (|make_list[55:1][67:0]| x0 x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|make_list[58:0]| x1) (and (= x0 0) (<= (+ 1 x1) 0))) (|make_list[58:1][61:0]| x1 x0))))
(assert (forall ((x0 Int)(x1 Int)) (=> (and (|make_list[55:0]| x1) (and (= (+ 1 x0) x1) (>= x1 0))) (|make_list[58:0]| x0))))
(assert (forall ((x0 Int)) (|make_list[55:0]| x0)))
(check-sat)
(get-model)
(exit)