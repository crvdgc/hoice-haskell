(set-logic HORN)

(declare-fun |loopb$u8|
  ( Bool Int Int Int ) Bool
)

(declare-fun |loopa$u4|
  ( Int Int Int Int ) Bool
)

(assert
  (forall ( (|$V-reftype:22| Int) (|$alpha-1:ax| Int) (|$alpha-2:ay| Int) (|$alpha-3:az| Int) (|$knormal:1| Bool) (|$knormal:2| Int) (|$knormal:4| Int) (|$knormal:7| Int) (|$knormal:9| Int) )
    (=>
      ( and (= |$knormal:7| (- |$alpha-3:az| 2)) (= |$knormal:4| (+ |$alpha-2:ay| 1)) (= |$knormal:2| (+ |$alpha-1:ax| 1)) (= |$knormal:1| (< |$alpha-1:ax| 10)) (= |$V-reftype:22| |$knormal:9|) |$knormal:1| (|loopa$u4| |$knormal:9| |$knormal:7| |$knormal:4| |$knormal:2|) true true true )
      (|loopa$u4| |$V-reftype:22| |$alpha-3:az| |$alpha-2:ay| |$alpha-1:ax|)
    )
  )
)
(assert
  (forall ( (|$alpha-1:ax| Int) (|$alpha-2:ay| Int) (|$alpha-3:az| Int) (|$knormal:1| Bool) (|$knormal:2| Int) (|$knormal:4| Int) (|$knormal:7| Int) )
    (=>
      ( and (= |$knormal:7| (- |$alpha-3:az| 2)) (= |$knormal:4| (+ |$alpha-2:ay| 1)) (= |$knormal:2| (+ |$alpha-1:ax| 1)) (= |$knormal:1| (< |$alpha-1:ax| 10)) |$knormal:1| true true true )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-1:ax| Int) (|$alpha-2:ay| Int) (|$alpha-3:az| Int) (|$knormal:1| Bool) (|$knormal:2| Int) (|$knormal:4| Int) (|$knormal:7| Int) )
    (=>
      ( and (= |$knormal:7| (- |$alpha-3:az| 2)) (= |$knormal:4| (+ |$alpha-2:ay| 1)) (= |$knormal:2| (+ |$alpha-1:ax| 1)) (= |$knormal:1| (< |$alpha-1:ax| 10)) |$knormal:1| true true true )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-1:ax| Int) (|$alpha-2:ay| Int) (|$alpha-3:az| Int) (|$knormal:1| Bool) (|$knormal:2| Int) (|$knormal:4| Int) (|$knormal:7| Int) )
    (=>
      ( and (= |$knormal:7| (- |$alpha-3:az| 2)) (= |$knormal:4| (+ |$alpha-2:ay| 1)) (= |$knormal:2| (+ |$alpha-1:ax| 1)) (= |$knormal:1| (< |$alpha-1:ax| 10)) |$knormal:1| true true true )
      true
    )
  )
)
(assert
  (forall ( (|$V-reftype:23| Int) (|$alpha-1:ax| Int) (|$alpha-2:ay| Int) (|$alpha-3:az| Int) (|$knormal:1| Bool) )
    (=>
      ( and (= |$knormal:1| (< |$alpha-1:ax| 10)) (= |$V-reftype:23| |$alpha-3:az|) (not |$knormal:1|) true true true )
      (|loopa$u4| |$V-reftype:23| |$alpha-3:az| |$alpha-2:ay| |$alpha-1:ax|)
    )
  )
)
(assert
  (not (exists ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) (|$alpha-15:rsx| Int) (|$knormal:20| Bool) (|$knormal:22| Bool) (|$knormal:27| Int) )
    ( and (= |$knormal:22| (= |$knormal:20| true)) (= |$alpha-15:rsx| 10) (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) (not |$knormal:22|) (|loopb$u8| |$knormal:20| |$knormal:27| |$alpha-15:rsx| |$alpha-15:rsx|) (|loopa$u4| |$knormal:27| |$alpha-13:z| |$alpha-12:y| |$alpha-11:x|) )
    )
  )
)
(assert
  (forall ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) (|$alpha-15:rsx| Int) (|$knormal:27| Int) )
    (=>
      ( and (= |$alpha-15:rsx| 10) (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) (|loopa$u4| |$knormal:27| |$alpha-13:z| |$alpha-12:y| |$alpha-11:x|) )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) (|$alpha-15:rsx| Int) (|$knormal:27| Int) )
    (=>
      ( and (= |$alpha-15:rsx| 10) (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) (|loopa$u4| |$knormal:27| |$alpha-13:z| |$alpha-12:y| |$alpha-11:x|) )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) (|$alpha-15:rsx| Int) (|$knormal:27| Int) )
    (=>
      ( and (= |$alpha-15:rsx| 10) (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) (|loopa$u4| |$knormal:27| |$alpha-13:z| |$alpha-12:y| |$alpha-11:x|) )
      true
    )
  )
)
(assert
  (forall ( (|$V-reftype:28| Bool) (|$alpha-4:bx| Int) (|$alpha-5:by| Int) (|$alpha-6:bz| Int) (|$alpha-7:rz| Int) (|$alpha-8:rx| Int) (|$alpha-9:ry| Int) (|$knormal:10| Bool) (|$knormal:15| Bool) )
    (=>
      ( and (= |$knormal:10| (> |$alpha-4:bx| 0)) (= |$alpha-9:ry| (- |$alpha-5:by| 1)) (= |$alpha-8:rx| (- |$alpha-4:bx| 1)) (= |$alpha-7:rz| (+ |$alpha-6:bz| 2)) (= |$V-reftype:28| |$knormal:15|) |$knormal:10| (|loopb$u8| |$knormal:15| |$alpha-7:rz| |$alpha-9:ry| |$alpha-8:rx|) true true true )
      (|loopb$u8| |$V-reftype:28| |$alpha-6:bz| |$alpha-5:by| |$alpha-4:bx|)
    )
  )
)
(assert
  (forall ( (|$alpha-4:bx| Int) (|$alpha-5:by| Int) (|$alpha-6:bz| Int) (|$alpha-7:rz| Int) (|$alpha-8:rx| Int) (|$alpha-9:ry| Int) (|$knormal:10| Bool) )
    (=>
      ( and (= |$knormal:10| (> |$alpha-4:bx| 0)) (= |$alpha-9:ry| (- |$alpha-5:by| 1)) (= |$alpha-8:rx| (- |$alpha-4:bx| 1)) (= |$alpha-7:rz| (+ |$alpha-6:bz| 2)) |$knormal:10| true true true )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-4:bx| Int) (|$alpha-5:by| Int) (|$alpha-6:bz| Int) (|$alpha-7:rz| Int) (|$alpha-8:rx| Int) (|$alpha-9:ry| Int) (|$knormal:10| Bool) )
    (=>
      ( and (= |$knormal:10| (> |$alpha-4:bx| 0)) (= |$alpha-9:ry| (- |$alpha-5:by| 1)) (= |$alpha-8:rx| (- |$alpha-4:bx| 1)) (= |$alpha-7:rz| (+ |$alpha-6:bz| 2)) |$knormal:10| true true true )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-4:bx| Int) (|$alpha-5:by| Int) (|$alpha-6:bz| Int) (|$alpha-7:rz| Int) (|$alpha-8:rx| Int) (|$alpha-9:ry| Int) (|$knormal:10| Bool) )
    (=>
      ( and (= |$knormal:10| (> |$alpha-4:bx| 0)) (= |$alpha-9:ry| (- |$alpha-5:by| 1)) (= |$alpha-8:rx| (- |$alpha-4:bx| 1)) (= |$alpha-7:rz| (+ |$alpha-6:bz| 2)) |$knormal:10| true true true )
      true
    )
  )
)
(assert
  (forall ( (|$V-reftype:29| Bool) (|$alpha-4:bx| Int) (|$alpha-5:by| Int) (|$alpha-6:bz| Int) (|$knormal:10| Bool) )
    (=>
      ( and (= |$knormal:10| (> |$alpha-4:bx| 0)) (= |$V-reftype:29| (> |$alpha-6:bz| (- 1))) (not |$knormal:10|) true true true )
      (|loopb$u8| |$V-reftype:29| |$alpha-6:bz| |$alpha-5:by| |$alpha-4:bx|)
    )
  )
)
(assert
  (forall ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) )
    (=>
      ( and (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) )
    (=>
      ( and (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) )
      true
    )
  )
)
(assert
  (forall ( (|$alpha-11:x| Int) (|$alpha-12:y| Int) (|$alpha-13:z| Int) )
    (=>
      ( and (= |$alpha-13:z| 0) (= |$alpha-12:y| 0) (= |$alpha-11:x| 0) )
      true
    )
  )
)
(check-sat)

(get-model)

(exit)

