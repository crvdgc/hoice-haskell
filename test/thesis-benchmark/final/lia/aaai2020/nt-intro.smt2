(set-info :origin "
(*
assume (x >= 0);
while (x >= 0) do
  if *E then x := x - 1 else x := x + 1;
assert false
*)
I(x) :- x >= 0.
I(x - 1), I(x + 1) :- I(x), x >= 0.
false :- I(x), x < 0.
")
(set-logic HORN)
(declare-fun I (Int) Bool)
(assert
    (forall ((x Int))
        (=>
            (>= x 0)
            (I x)
        )
    )
)
(assert
    (forall ((x Int) (xm1 Int) (xp1 Int))
        (=>
            (and (>= x 0) (I x) (= xm1 (- x 1)) (= xp1 (+ x 1))
            )
            (or (I xm1) (I xp1))
        )
    )
)
(assert
    (forall ((x Int))
        (=>
            (and (I x) (< x 0))
            false
        )
    )
)
