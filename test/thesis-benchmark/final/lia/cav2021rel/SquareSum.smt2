(set-logic HORN)

(declare-fun Inv (Int Int Int Int Int Int) Bool)
(declare-fun SchFT (Int Int Int Int Int Int) Bool)
(declare-fun SchTT (Int Int Int Int Int Int) Bool)
(declare-fun SchTF (Int Int Int Int Int Int) Bool)
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int))
  (=> (and (Inv F D B E C A) (>= F D) (>= E C) (>= A B)) false)
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int) (H Int))
       (=> (and (SchFT H E C G D B)
                     (Inv H E C G D B)
                     (or (and (>= G D) (= F G) (= A B)) (and (< G D) (= F (+ G 1)) (= A (+ B (* G G))))))
                     (Inv H E C F D A))
    ))
(assert (forall ((A Int)
         (B Int)
         (C Int)
         (D Int)
         (E Int)
         (F Int)
         (G Int)
         (H Int)
         (I Int)
         (J Int))
       (=> (and (SchTT J F D H E B)
                     (Inv J F D H E B)
                     (or (and (>= H E) (= G H) (= A B)) (and (< H E) (= G (+ H 1)) (= A (+ B (* H H)))))
                     (or (and (>= J F) (= I J) (= C D)) (and (< J F) (= I (+ J 1)) (= C (+ D (* J J))))))
            (Inv I F C G E A))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int) (H Int))
       (=> (and (SchTF H E C F D A)
                     (Inv H E C F D A)
                     (or (and (>= H E) (= G H) (= B C)) (and (< H E) (= G (+ H 1)) (= B (+ C (* H H))))))
        (Inv G E B F D A))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int))
  (=> (and (Inv F D B E C A) (or (< F D) (< E C)))
      (or (SchTT F D B E C A) (SchFT F D B E C A) (SchTF F D B E C A)))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int))
  (=> (and (< F E) (< F D) (< E C) (< C D) (< 0 F) (< 0 E) (= B 0) (= A 0))
         (Inv F D B E C A))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int))
  (=> (and (SchTF F D B E C A) (Inv F D B E C A) (>= F D) (< E C)) false)
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int))
  (=> (and (SchFT F D B E C A) (Inv F D B E C A) (>= E C) (< F D)) false)
    ))


(check-sat)
(exit)