(set-logic HORN)
; (set-option :produce-proofs true)
(declare-const x Int)
(assert (exists ((x Int)) (not (> x 0))))

(check-sat)
(get-model)


