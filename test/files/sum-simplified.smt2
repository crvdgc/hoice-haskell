
(set-info :origin "Verification conditions for the caml program
  (*
  USED: PLDI2011 as sum
  USED: PEPM2013 as sum
  *)
  let rec sum n =
    if n <= 0
    then 0
    else n + sum (n-1)
  
  let main n =
    assert (n <= sum n)

(assert
  (forall ( (|$V-reftype:8| Int) (|$alpha-1:n| Int) (|$knormal:1| Int) )
    (=>
      ( and (= (not (= 0 |$knormal:1|)) (<= |$alpha-1:n| 0)) (= |$V-reftype:8| 0) (not (= 0 |$knormal:1|)) true )
      (|sum$unknown:2| |$V-reftype:8| |$alpha-1:n|)
    )
  )
)
(assert (not
  (exists ( (|$V-reftype:8| Int) (|$alpha-1:n| Int) (|$knormal:1| Int) )
      ( and (= (not (= 0 |$knormal:1|)) (<= |$alpha-1:n| 0)) (not (= 0 |$knormal:1|)))
  )
))
")

(set-logic HORN)

(declare-fun |sum$unknown:2|
  ( Int Int ) Bool
)

(assert (not
  (exists ( (|$V-reftype:8| Int) (|$alpha-1:n| Int) (|$knormal:1| Int) )
      ( and (= (not (= 0 |$knormal:1|)) (<= |$alpha-1:n| 0)) (not (= 0 |$knormal:1|)))
  )
))
(check-sat)

(get-model)

(exit)

