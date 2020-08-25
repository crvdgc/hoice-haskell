(set-logic LIA)
(declare-const x Int)
(declare-const y Int)
(assert
    (not
        (and
            (=>
                (< (+ x 1) 3)
                (< x 2)
            )
            (< y 0)
        )
    )
)
(check-sat)
(get-model)
