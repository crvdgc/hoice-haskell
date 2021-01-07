(set-logic HORN)

(declare-fun f (Int Int) Bool)

(assert
    (forall ((x Int))
        (=>
            (f x 1)
            false
        )
    )
)

(check-sat)
(exit)

