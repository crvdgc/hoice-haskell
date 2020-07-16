(set-logic HORN)
(set-option :produce-unsat-cores true)
(declare-fun f (Int) Bool)

(assert
    (!
        (forall ((x Int))
            (=>
                true
                (f x)
            )
        )
    :named one)
)

(assert
    (!
        (forall ((x Int))
            (=>
                (f x)
                true
            )
        )
    :named three)
)

(assert
    (!
        (forall ((x Int))
            (=>
                (f x)
                false
            )
        )
    :named two)
)

(check-sat)
(get-unsat-core)

