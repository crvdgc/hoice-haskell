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
")

(set-logic HORN)

(assert (forall ((x Int) ) (=> (= (= x 0) (= x 0)) true)))
(check-sat)

(get-model)

(exit)
