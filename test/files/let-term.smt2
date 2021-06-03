; Instance after running `cfg_red`.

(set-logic HORN)

; Datatypes

; Functions

; Side-clauses


(declare-fun
  |init$unknown:6|
  ( Int Int Int Int ) Bool
)
(declare-fun
  |init$unknown:5|
  ( Int Int Int ) Bool
)
(declare-fun
  |init$unknown:4|
  ( Int Int Int Int ) Bool
)
(declare-fun
  |init$unknown:3|
  ( Int Int Int ) Bool
)

; Original clauses' names (0) {
; }

; Clause #0
;   from: #14
;   6 inactive variable(s)
;   unroll: false
;   terms_changed: false
;   preds_changed: false
;   created by `parsing`
(assert 
  (forall
    ( (|$alpha-7:j| Int) (|$knormal:5| Int) (|$knormal:6| Int) (|$knormal:7| Int) (hoice_fresh_var@8 Int) (hoice_fresh_var@9 Int) )
    (let
      ( (v_10 (+ 1 |$alpha-7:j|)) )
      (=>
        (and
          (>= (+ (* (- 1) |$alpha-7:j|) hoice_fresh_var@9) 1) (not (= |$knormal:7| 0)) (not (= |$knormal:6| 0)) (not (= |$knormal:5| 0)) (>= (+ (* (- 1) |$alpha-7:j|) hoice_fresh_var@8) 1)
          (|init$unknown:3| |$alpha-7:j| hoice_fresh_var@8 v_10) (|init$unknown:3| |$alpha-7:j| hoice_fresh_var@9 v_10)
        )
        (|init$unknown:4| 1 |$alpha-7:j| hoice_fresh_var@9 v_10)
      )
    )
  )
)

(check-sat)
