(set-logic HORN)
(set-info :origin "
(*
assume (x >= 0 /\ y = 0);
(while x > 0 do
  if *E then y := y + x; x := x - 1);
assert false
*)
I(x, y) :- x >= 0, y = 0.
I(x, y), I(x - 1, y + x) :- I(x, y), x > 0.
false :- I(x, y), x <= 0.

Note: program unsafe, counterexample: I(0, 0)
")
(declare-fun I (Int Int) Bool)
(assert
    (forall ((x Int) (y Int))
        (=>
            (and (>= x 0) (= y 0))
            (I x y)
        )
    )
)
(assert
    (forall ((x Int) (xm1 Int) (xpy Int))
        (=>
            (and (> x 0) (I x y) (= xm1 (- x 1)) (= xpy (+ x y))
            )
            (or (I x y) (I xm1 xpy))
        )
    )
)
(assert
    (forall ((x Int) (y Int))
        (=>
            (and (I x y) (<= x 0))
            false
        )
    )
)
(check-sat)
(exit)
