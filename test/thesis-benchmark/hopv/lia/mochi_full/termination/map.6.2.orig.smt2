(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/termination/map.6.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_20592[153:0]| ( Int) Bool)
(declare-fun |map[59:7][130:3]| ( Bool  Int  Int  Int  Bool  Int  Int  Bool  Int  Int  Int) Bool)
(declare-fun |map[116:7][124:3]| ( Bool  Int  Int  Int  Bool  Int  Int  Bool  Int  Int  Int) Bool)
(declare-fun |map[116:11]| ( Bool  Int  Int  Int  Bool  Int  Int  Bool  Int  Int  Int) Bool)
(declare-fun |map[59:7][67:4][110:0]| ( Bool  Int  Int  Int  Bool  Int  Int  Bool  Int  Int  Int  Int) Bool)
(declare-fun |map[59:7][67:3]| ( Bool  Int  Int  Int  Bool  Int  Int  Bool  Int  Int  Int) Bool)
(declare-fun |map[59:11]| ( Bool  Int  Int  Int  Bool  Int  Int  Bool  Int  Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_20592[153:0]| x0))))
(assert (forall ((x0 Int)(x1 Int)(x2 Int)(var121 Int)) (=> (|map[59:7][130:3]| false 0 0 0 false 0 0 true x1 x2 var121) (|fail_20592[153:0]| x0))))
(assert (forall ((x9 Bool)(x8 Int)(x7 Int)(x0 Int)(x6 Bool)(x5 Int)(x4 Int)(x11 Bool)(x12 Int)(x13 Int)(x14 Int)(x10 Int)(x3 Bool)(x1 Int)(x2 Int)(var122 Int)(var123 Int)) (=> (and (|map[116:7][124:3]| x3 x1 x2 0 x3 x1 x2 x11 x12 x13 x14) (and (|map[59:11]| x9 x8 x7 x0 x6 x5 x4 x3 x1 x2 x10) (and (|map[59:7][67:4][110:0]| x9 x8 x7 x0 x6 x5 x4 x3 x1 x2 var122 var123) (not (= x10 0))))) (|map[59:7][130:3]| x9 x8 x7 x0 x6 x5 x4 x11 x12 x13 x14))))
(assert (forall ((x9 Bool)(x8 Int)(x7 Int)(x0 Int)(x6 Bool)(x5 Int)(x4 Int)(x11 Bool)(x12 Int)(x13 Int)(x14 Int)(x10 Int)) (=> (and (|map[116:11]| x9 x8 x7 x0 x6 x5 x4 x11 x12 x13 x10) (not (= x10 0))) (|map[116:7][124:3]| x9 x8 x7 x0 x6 x5 x4 x11 x12 x13 x14))))
(assert (forall ((x11 Bool)(x9 Int)(x10 Int)(x0 Int)(x6 Bool)(x4 Int)(x2 Int)(x7 Bool)(x5 Int)(x3 Int)(x1 Int)(x19 Int)(x18 Bool)(x17 Int)(x16 Int)(x8 Int)(x15 Bool)(x14 Int)(x13 Int)(x12 Int)(x20 Int)) (=> (and (|map[59:11]| x18 x17 x16 x8 x15 x14 x13 x11 x9 x10 x19) (and (|map[59:7][67:4][110:0]| x18 x17 x16 x8 x15 x14 x13 x11 x9 x10 x12 x20) (and (= x0 0) (and (= (+ 1 x1) x19) (and (not (= x19 0)) (and (= x4 x9) (and (= x4 x5) (and (= x2 x10) (and (= x2 x3) (and (= x6 x11) (= x6 x7))))))))))) (|map[116:11]| x11 x9 x10 x0 x6 x4 x2 x7 x5 x3 x1))))
(assert (forall ((x6 Bool)(x7 Int)(x8 Int)(x0 Int)(x3 Bool)(x2 Int)(x1 Int)(x9 Bool)(x10 Int)(x11 Int)(x12 Int)(x13 Int)) (=> (and (|map[59:7][67:3]| x6 x7 x8 0 x6 x7 x8 x9 x10 x11 x12) (and (= x0 0) (and (= x7 0) (and (= x8 0) (and (= x13 (+ 3 x12)) (and (= x2 x7) (and (= x1 x8) (and (not x9) (and (not x6) (not x3)))))))))) (|map[59:7][67:4][110:0]| x6 x7 x8 x0 x3 x2 x1 x9 x10 x11 x12 x13))))
(assert (forall ((x9 Bool)(x8 Int)(x7 Int)(x0 Int)(x6 Bool)(x5 Int)(x4 Int)(x11 Bool)(x12 Int)(x13 Int)(x14 Int)(x10 Int)) (=> (and (|map[59:11]| x9 x8 x7 x0 x6 x5 x4 x11 x12 x13 x10) (not (= x10 0))) (|map[59:7][67:3]| x9 x8 x7 x0 x6 x5 x4 x11 x12 x13 x14))))
(assert (forall ((x9 Bool)(x7 Int)(x8 Int)(x0 Int)(x5 Bool)(x3 Int)(x1 Int)(x6 Bool)(x4 Int)(x2 Int)(x11 Int)) (=> (and (= x0 0) (and (= x7 0) (and (= x8 0) (and (>= x11 0) (and (= x3 x7) (and (= x3 x4) (and (= x1 x8) (and (= x1 x2) (and (not x9) (and (not x5) (= x5 x6))))))))))) (|map[59:11]| x9 x7 x8 x0 x5 x3 x1 x6 x4 x2 x11))))
(check-sat)
(get-model)
(exit)
