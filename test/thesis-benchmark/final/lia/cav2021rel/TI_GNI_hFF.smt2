(set-logic HORN)

(declare-fun Inv (Int Int Int Int Int Int Int) Bool)
(declare-fun SchFT (Int Int Int Int Int Int Int) Bool)
(declare-fun SchTT (Int Int Int Int Int Int Int) Bool)
(declare-fun SchTF (Int Int Int Int Int Int Int) Bool)
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int))
  (=> (and (Inv C G B E F A D)
              (= C B)
              (not (> G 0))
              (not (> F 0))
              (not (= B A)))
         false)
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
         (J Int)
         (K Int))
       (=> (and (SchFT E K D G J C F)
                (Inv E K D G J C F)
                (or (and (> J 0)
                         (> I 0)
                         (= B (+ C 1))
                         (= A C)
                         (not (> H 0)))
                    (and (= B C)
                         (= A C)
                         (not (> J 0))
                         (not (> I 0))
                         (not (> H 0)))))
           (or (Inv E K D G H A F) (Inv E K D G I B F)))
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
         (J Int)
         (K Int)
         (L Int)
         (M Int))
       (=> (and (SchTT F M E H K C G)
                (Inv F M E H K C G)
                (or (and (> K 0)
                         (> J 0)
                         (= B (+ C 1))
                         (= A C)
                         (not (> I 0)))
                    (and (= B C)
                         (= A C)
                         (not (> K 0))
                         (not (> J 0))
                         (not (> I 0))))
                (or (and (> M 0) (or (and (> L 0) (= D (+ E 1)))
                    (and (= D E) (not (> L 0)))))
                    (and (= D E) (not (> M 0)) (not (> L 0))))
                )
           (or (Inv F L D H I A G) (Inv F L D H J B G)) )
    ))
(assert (forall ((A Int)
         (B Int)
         (C Int)
         (D Int)
         (E Int)
         (F Int)
         (G Int)
         (H Int)
         (I Int))
       (=> (and (SchTF D I C F G A E) (Inv D I C F G A E) (or (and (> I 0) (or (and (> H 0) (= B (+ C 1)))
                    (and (= B C) (not (> H 0)))))
                    (and (= B C) (not (> I 0)) (not (> H 0)))))
           (Inv D H B F G A E))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int))
       (=> (and (Inv C G B E F A D)
                     (or (> F 0) (> G 0) (not (= C B))))
         (or (SchTT C G B E F A D) (SchFT C G B E F A D) (SchTF C G B E F A D)))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int))
  (=> (and (> G 0) (> F 0) (= E D) (= B E) (= A D))
         (Inv C G B E F A D))
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int))
  (=> (and (SchTF C G B E F A D)
              (Inv C G B E F A D)
              (not (> G 0))
              (= C B)
              (> F 0))
         false)
    ))
(assert (forall ((A Int) (B Int) (C Int) (D Int) (E Int) (F Int) (G Int))
       (=>
       (and (SchFT C G B E F A D)
                     (Inv C G B E F A D)
                     (not (> F 0))
                     (or (> G 0) (not (= C B))))
                     false)
    ))


(check-sat)
(exit)
