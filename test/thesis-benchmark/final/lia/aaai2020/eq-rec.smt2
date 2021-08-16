(set-info :origin "
(* 
let rec loop x y =
  if x <> 0 then
    if *E then loop (x - 1) y
    else loop (x - 1) (y + 1)
  else y
let main x =
  let z = loop x 0 in assert (z = x)
*)
P(x, y, z1), P(x, y, z2) :- x \= 0, P(x - 1, y, z1), P(x - 1, y + 1, z2).
P(x, y, z) :- x = 0, z = y.
z = x :- P(x, 0, z).
")
(set-logic HORN)
(declare-fun P (Int Int Int) Bool)
(assert
    (forall ((x Int) (y Int) (z1 Int) (z2 Int) (xm1 Int) (yp1 Int))
        (=>
            (and (= xm1 (- x 1)) (= yp1 (+ y 1)) (not (= x 0)) (P xm1 y z1) (P xm1 yp1 z2))
            (or (P x y z1) (P x y z2))
        )
    )
)
(assert
    (forall ((x Int) (y Int) (z Int))
        (=>
            (and (= x 0) (= z y))
            (P x y z)
        )
    )
)
(assert
    (forall ((x Int) (y Int) (z Int))
        (=>
            (and (= y 0) (P x y z) (not (= z x)))
            false
        )
    )
)
