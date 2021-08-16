(set-info :origin "
(* 
assume (x = x0 /\ y = 0}
(while x != 0 do
  x := x - 1; if *E then y := y + 1);
assert (y = x0)
*)
I(x0, x, y) :- x = x0, y = 0.
I(x0, x - 1, y + 1), I(x0, x - 1, y) :- I(x0, x, y), x \= 0.
y = x0 :- I(x0, x, y), x = 0.
")
(set-logic HORN)
(declare-fun I (Int Int Int) Bool)
(assert
    (forall ((x0 Int) (x Int) (z Int))
        (=>
            (and (= x x0) (= y 0))
            (I x0 x y)
        )
    )
)
(assert
    (forall ((x0 Int) (x Int) (y Int) (xm1 Int) (yp1 Int))
        (=>
            (and
                (not (= x 0)) (= (- x 1) xm1) (= (+ y 1) yp1)
                (I x0 x y)
            )
            (or (I x0 xm1 yp1) (I x0 xm1 y))
        )
    )
)
(assert
    (forall ((x0 Int) (x Int) (y Int))
        (=>
            (and (I x0 x y) (not (= y x0)) (= x 0))
            false
        )
    )
)
