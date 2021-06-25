(set-logic HORN)
(set-info :source |
  Benchmark: test_safe_2019/adt/fun_list.ml
  Generated by MoCHi
|)
(set-info :status unknown)
(declare-fun |fail_53[507:0]| ( Int) Bool)
(declare-fun |for_all[412:3][502:0]| ( Int  Bool) Bool)
(declare-fun |for_all[485:3][488:0]| ( Int  Bool) Bool)
(declare-fun |for_all[485:1]| ( Int) Bool)
(declare-fun |for_all[412:0][469:1][475:0]| ( Int  Bool) Bool)
(declare-fun |for_all[412:0][469:0]| ( Int) Bool)
(declare-fun |for_all[412:2][420:1][455:0]| ( Int  Int  Int) Bool)
(declare-fun |map[273:3][408:1][424:1][451:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |map[273:3][408:1][424:0]| ( Int  Int  Int) Bool)
(declare-fun |for_all[412:2][420:0]| ( Int  Int) Bool)
(declare-fun |for_all[412:1]| ( Int) Bool)
(declare-fun |map[273:3][408:0]| ( Int  Int) Bool)
(declare-fun |map[273:0][320:1][386:0]| ( Int) Bool)
(declare-fun |map[273:0][320:0][326:1][380:0]| ( Int  Int) Bool)
(declare-fun |map[273:2][281:1][297:0][344:1][362:0]| ( Int  Int  Int  Int) Bool)
(declare-fun |map[273:2][281:1][297:0][344:0]| ( Int  Int  Int) Bool)
(declare-fun |map[273:2][281:0]| ( Int  Int) Bool)
(declare-fun |map[305:3][308:0]| ( Int  Int) Bool)
(declare-fun |map[273:0][320:0][326:0]| ( Int) Bool)
(declare-fun |map[305:1]| ( Int) Bool)
(declare-fun |map[273:1]| ( Int) Bool)
(assert (not (exists ((x0 Int)) (|fail_53[507:0]| x0))))
(assert (forall ((x0 Int)(x2 Int)) (=> (and (|for_all[412:3][502:0]| x2 false) (|map[273:3][408:0]| 3 x2)) (|fail_53[507:0]| x0))))
(assert (forall ((x1 Int)(x2 Bool)(var309 Int)(var312 Bool)(var310 Bool)(var311 Int)) (=> (and (|for_all[485:3][488:0]| var309 var310) (and (|for_all[412:2][420:1][455:0]| x1 0 var311) (and (|for_all[412:1]| x1) (and (|for_all[412:0][469:1][475:0]| var311 var312) (and (<= 1 x1) (and (= (+ 1 var309) x1) (= x2 (and var312 var310)))))))) (|for_all[412:3][502:0]| x1 x2))))
(assert (forall ((x1 Int)(x0 Bool)) (=> (and (|for_all[485:1]| x1) (and (<= x1 0) x0)) (|for_all[485:3][488:0]| x1 x0))))
(assert (forall ((x1 Int)(x2 Int)(x0 Int)(x3 Bool)) (=> (and (|for_all[412:2][420:1][455:0]| x2 0 x0) (and (|for_all[412:1]| x2) (and (|for_all[412:0][469:1][475:0]| x0 x3) (and (= (+ 1 x1) x2) (>= x2 1))))) (|for_all[485:1]| x1))))
(assert (forall ((x2 Int)(x3 Bool)(x1 Int)) (=> (and (|for_all[412:0][469:0]| x2) (and (|map[273:3][408:0]| 3 x1) (= x3 (>= x2 0)))) (|for_all[412:0][469:1][475:0]| x2 x3))))
(assert (forall ((x2 Int)(var313 Int)) (=> (and (|for_all[412:2][420:1][455:0]| var313 0 x2) (and (|for_all[412:1]| var313) (<= 1 var313))) (|for_all[412:0][469:0]| x2))))
(assert (forall ((x3 Int)(x1 Int)(x2 Int)) (=> (and (|for_all[412:1]| x3) (and (|map[273:3][408:1][424:1][451:0]| 3 x3 x1 x2) (|map[273:3][408:0]| 3 x3))) (|for_all[412:2][420:1][455:0]| x3 x1 x2))))
(assert (forall ((x3 Int)(x4 Int)(x1 Int)(x2 Int)(var317 Int)(var319 Int)(var318 Int)) (=> (and (|map[273:3][408:1][424:0]| x3 var317 x1) (and (|map[305:3][308:0]| var318 var319) (and (|map[273:3][408:0]| x3 x4) (and (|map[273:0][320:1][386:0]| x2) (and (|map[273:1]| x3) (and (= var317 (+ 1 var319)) (and (= x4 (+ 1 var319)) (and (= (+ 1 var318) x3) (and (= x1 0) (<= 1 x3)))))))))) (|map[273:3][408:1][424:1][451:0]| x3 x4 x1 x2))))
(assert (forall ((x3 Int)(x1 Int)(x2 Int)) (=> (and (|map[273:1]| x3) (and (|for_all[412:2][420:0]| x1 x2) (and (|map[273:3][408:0]| 3 x1) (= x3 3)))) (|map[273:3][408:1][424:0]| x3 x1 x2))))
(assert (forall ((x1 Int)(x2 Int)) (=> (and (|for_all[412:1]| x1) (and (= x2 0) (<= 1 x1))) (|for_all[412:2][420:0]| x1 x2))))
(assert (forall ((x1 Int)) (=> (|map[273:3][408:0]| 3 x1) (|for_all[412:1]| x1))))
(assert (forall ((x1 Int)(x2 Int)(var320 Int)(var321 Int)(var322 Int)) (=> (and (|map[305:3][308:0]| var320 var321) (and (|map[273:0][320:1][386:0]| var322) (and (|map[273:1]| x1) (and (<= 1 x1) (and (= (+ 1 var320) x1) (= x2 (+ 1 var321))))))) (|map[273:3][408:0]| x1 x2))))
(assert (forall ((x1 Int)) (=> (|map[273:0][320:0][326:1][380:0]| 0 x1) (|map[273:0][320:1][386:0]| x1))))
(assert (forall ((x2 Int)(x3 Int)(var323 Int)(var325 Int)(var324 Int)) (=> (and (|map[305:3][308:0]| var323 var324) (and (|map[273:2][281:1][297:0][344:1][362:0]| var325 0 x2 x3) (and (|map[273:1]| var325) (and (= (+ 1 var323) var325) (<= 1 var325))))) (|map[273:0][320:0][326:1][380:0]| x2 x3))))
(assert (forall ((x4 Int)(x1 Int)(x2 Int)(x3 Int)) (=> (and (|map[273:1]| x4) (and (|map[273:2][281:0]| 3 x1) (and (|map[273:2][281:1][297:0][344:0]| 3 1 x2) (and (= x4 3) (and (= x1 1) (= x3 (+ 1 x2))))))) (|map[273:2][281:1][297:0][344:1][362:0]| x4 x1 x2 x3))))
(assert (forall ((x2 Int)(x3 Int)(x1 Int)(var326 Int)(var327 Int)) (=> (and (|map[273:2][281:0]| x2 x3) (and (|map[305:3][308:0]| var326 var327) (and (|map[273:0][320:0][326:0]| x1) (and (|map[273:1]| x2) (and (= x3 0) (and (<= 1 x2) (= (+ 1 var326) x2))))))) (|map[273:2][281:1][297:0][344:0]| x2 x3 x1))))
(assert (forall ((x1 Int)) (=> (= x1 0) (|map[273:0][320:0][326:0]| x1))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|map[305:1]| x1) (and (= x0 0) (<= x1 0))) (|map[305:3][308:0]| x1 x0))))
(assert (forall ((x1 Int)(x0 Int)) (=> (and (|map[273:1]| x0) (and (= (+ 1 x1) x0) (>= x0 1))) (|map[305:1]| x1))))
(assert (forall ((x1 Int)(x2 Int)) (=> (and (|map[273:1]| x1) (and (= x2 0) (<= 1 x1))) (|map[273:2][281:0]| x1 x2))))
(assert (forall ((x0 Int)) (=> (= x0 3) (|map[273:1]| x0))))
(check-sat)
(get-model)
(exit)
