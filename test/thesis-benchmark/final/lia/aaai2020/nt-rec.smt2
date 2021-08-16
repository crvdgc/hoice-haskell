(set-info :origin "
(*
let rec loop x =
  if x >= 0 then
    (if *E then loop (x-1) else loop (x+1))
  else x
let main x =
  assume (x >= 0); loop x; assert false
*)
P(x,y) :- x < 0, x = y.
P(x, y1), P(x, y2) :- x >= 0, P(x-1, y1), P(x+1, y2).
false :- x >= 0, P(x, y).
")
(set-logic HORN)
(declare-fun P (Int Int) Bool)
(assert
    (forall ((x Int))
        (=>
            (and (< x 0) (= x y))
            (P x y)
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
    (forall ((x0 Int) (x Int) (y Int))
        (=>
            (and (I x) (< x 0))
            false
        )
    )
)
