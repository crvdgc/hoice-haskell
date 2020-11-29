(set-info :origin "handwritten test case for fib")

(set-logic HORN)
(declare-fun rho1 (Int) Bool)
(declare-fun rho2 (Int Int) Bool)
(assert
    (forall ( (n Int) (r Int) )
        (=>
            (and
                (rho1 n)
                (< n 2)
                (= r 1)
            )
            (rho2 n r)
        )
    )
)
(assert
    (forall ( (n Int) (r Int) (r1 Int) (r2 Int) (n1 Int) (n2 Int) )
        (=>
            (and
                (rho1 n)
                (>= n 2)
                (rho2 n1 r1)
                (rho2 n2 r2)
                (= (- n 1) n1)
                (= (- n 2) n2)
                (= (+ r1 r2) r)
            )
            (rho2 n r)
        )
    )
)
(assert
    (forall ( (n Int) )
        (=>
            true
            (rho1 n)
        )
    )
)
(assert
    (forall ( (n Int) (r Int) )
        (=>
            (and
                (rho1 n)
                (rho2 n r)
            )
            (<= n r)
        )
    )
)
(check-sat)
(get-model)
(exit)
