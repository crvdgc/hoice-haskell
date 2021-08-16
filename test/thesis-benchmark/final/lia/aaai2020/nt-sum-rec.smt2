(set-logic HORN)
(set-info :origin "
(*
let rec sum x =
  if x > 0 then
    (if *E then x + sum (x-1) else sum x)
  else 0
let main x =
  assume (x > 0); sum x; assert false
*)
P(x, x + y1), P(x, y2) :- x > 0, P(x - 1, y1), P(x, y2).
P(x, y) :- x <= 0, y = 0.
false :- x > 0, P(x, y).
")
(declare-fun P (Int Int) Bool)
(assert
    (forall ((x Int) (y Int) (y1 Int) (y2 Int) (xm1 Int) (xpy1 Int))
        (=>
            (and (> x 0) (P xm1 y1) (P x y2)
                 (= xpy1 (+ x y1)) (= xm1 (- x 1))
            )
            (or (P x xpy1) (P x y2))
        )
    )
)
(assert
    (forall ((x Int) (y Int))
        (=>
            (and (<= x 0) (= y 0))
            (P x y)
        )
    )
)
(assert
    (forall ((x Int) (y Int))
        (=>
            (and (> x 0) (P x y))
            false
        )
    )
)
(check-sat)
(exit)
