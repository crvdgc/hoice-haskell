(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/termination/Ackermann.9.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_20621[145:0]| ( Int) Bool)
(declare-fun |ack_without_checking_92[131:7]| ( Bool  Int  Int  Int  Bool  Int  Int  Int) Bool)
(declare-fun |ack_without_checking_92[111:7]| ( Bool  Int  Int  Int  Bool  Int  Int  Int) Bool)
(declare-fun |ack_without_checking_92[105:7]| ( Bool  Int  Int  Int  Bool  Int  Int  Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_20621[145:0]| x0))))
(assert (forall ((x0 Int)(x7 Int)(var239 Bool)(var240 Int)(var241 Int)(var242 Bool)(var243 Int)(var244 Int)) (=> (and (|ack_without_checking_92[131:7]| var239 var240 var241 x7 var242 var243 var244 0) (<= (+ 1 x7) 0)) (|fail_20621[145:0]| x0))))
(assert (forall ((x5 Bool)(x6 Int)(x7 Int)(x0 Int)(x2 Bool)(x3 Int)(x4 Int)(x1 Int)(var245 Bool)(var246 Int)(var247 Int)(var248 Bool)(var249 Int)(var250 Int)) (=> (and (|ack_without_checking_92[111:7]| var245 var246 var247 x6 var248 var249 var250 x7) (and (= x7 0) (and (= x1 1) (and (not (= x6 0)) (and (= (+ 1 x0) x6) (and (= x7 x4) (and (= x6 x3) (and x5 (and x2 (or (and (>= x3 (+ 1 x0)) (>= x0 0)) (and (>= x3 x0) (and (>= x4 (+ 1 x1)) (>= x1 0))))))))))))) (|ack_without_checking_92[131:7]| x5 x6 x7 x0 x2 x3 x4 x1))))
(assert (forall ((x5 Bool)(x9 Int)(x13 Int)(x1 Int)(x4 Bool)(x0 Int)(x2 Int)(x3 Int)(x6 Bool)(x7 Int)(x8 Int)(x10 Bool)(x11 Int)(x12 Int)) (=> (and (|ack_without_checking_92[105:7]| x6 x7 x8 x9 x10 x11 x12 x13) (and (= (+ 1 x3) x13) (and (not (= x9 0)) (and (= x13 x2) (and (= x0 x9) (and (= x0 x1) (and (not (= x13 0)) (and x4 x5)))))))) (|ack_without_checking_92[111:7]| x5 x9 x13 x1 x4 x0 x2 x3))))
(assert (forall ((x6 Bool)(x4 Int)(x5 Int)(x3 Int)(x2 Bool)(x1 Int)(x0 Int)(x8 Int)) (=> (and (= x4 0) (and (= x5 0) (and (>= x8 1) (and (>= x3 1) (and (= x5 x0) (and (= x4 x1) (and (not x6) (not x2)))))))) (|ack_without_checking_92[105:7]| x6 x4 x5 x3 x2 x1 x0 x8))))
(check-sat)
(get-model)
(exit)