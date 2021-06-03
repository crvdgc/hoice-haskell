; Instance before pre-processing.

(set-logic HORN)

; Datatypes

; Functions

; Side-clauses


(declare-fun
  |rev$unknown:5|
  ( Int Int ) Bool
)
(declare-fun
  |append$unknown:3|
  ( Int Int Int ) Bool
)

; Original clauses' names (0) {
; }

; Clause #0
;   from: #0
;   6 inactive variable(s)
;   unroll: false
;   terms_changed: true
;   preds_changed: true
;   created by `parsing`
(assert 
  (forall
    ( (|$V-reftype:17| Int) (|$alpha-1:x| Int) (|$alpha-2:y| Int) (|$knormal:1| Int) (|$knormal:2| Int) (|$knormal:5| Int) )
    (=>
      (and
        (= (+ (- 1) |$V-reftype:17| (* (- 1) |$knormal:5|)) 0)
        (|append$unknown:3| |$knormal:5| |$alpha-2:y| |$knormal:2|)
      )
      (|append$unknown:3| |$V-reftype:17| |$alpha-2:y| |$alpha-1:x|)
    )
  )
)


