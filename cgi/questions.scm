;-*-Scheme-*-

(define (get-q n)  ;; the number is 1-origin
  ;; returns #f if n-th problem doesn't exist
      (let ((n (- n 1)))
	(and (< -1 n (vector-length qdb))  ; range check
	     (vector-ref qdb n))))

(define qdb
#(;; Nat
					
  ; learn how to do addition
  (Nat "Z plus Z is Z")
  (Nat "Z plus S(S(Z)) is S(S(Z))")
  (Nat "S(S(Z)) plus Z is S(S(Z))")	
  (Nat "S(Z) plus S(S(S(Z))) is S(S(S(S(Z))))")	

  ; learn how to do multiplication
  (Nat "Z times S(S(Z)) is Z")
  (Nat "S(S(Z)) times Z is Z")
  (Nat "S(S(Z)) times S(Z) is S(S(Z))")
  (Nat "S(S(Z)) times S(S(Z)) is S(S(S(S(Z))))")

  ;; CompareNat
  (CompareNat1 "S(S(Z)) is less than S(S(S(Z)))")
  (CompareNat2 "S(S(Z)) is less than S(S(S(Z)))")
  (CompareNat3 "S(S(Z)) is less than S(S(S(Z)))")

  (CompareNat1 "S(S(Z)) is less than S(S(S(S(S(Z)))))")
  (CompareNat2 "S(S(Z)) is less than S(S(S(S(S(Z)))))")
  (CompareNat3 "S(S(Z)) is less than S(S(S(S(S(Z)))))")

  ;; EvalNatExp
  ; learn how to evaluate expressions
  (EvalNatExp "Z + S(S(Z)) evalto S(S(Z))")
  (EvalNatExp "S(S(Z)) + Z evalto S(S(Z))")
  (EvalNatExp "S(Z) + S(Z) + S(Z) evalto S(S(S(Z)))")
  (EvalNatExp "S(S(S(Z))) + S(S(Z)) * S(Z) evalto S(S(S(S(S(Z)))))")
  (EvalNatExp "(S(S(Z)) + S(S(Z))) * Z evalto Z")
  (EvalNatExp "Z * (S(S(Z)) + S(S(Z))) evalto Z")

  ;; ReduceNatExp
  ; learn how to reduce an expression to another.
  (ReduceNatExp "Z + S(S(Z)) -*-> S(S(Z))")
  (ReduceNatExp "S(Z) * S(Z) + S(Z) * S(Z) -d-> S(Z) + S(Z) * S(Z)")
  (ReduceNatExp "S(Z) * S(Z) + S(Z) * S(Z) ---> S(Z) * S(Z) + S(Z)")
  (ReduceNatExp "S(Z) * S(Z) + S(Z) * S(Z) -*-> S(S(Z))")

  ;; EvalML1
  (EvalML1 "3 + 5 evalto 8")
  (EvalML1 "8 - 2 - 3 evalto 3")
  (EvalML1 "(4 + 5) * (1 - 10) evalto -81")
  (EvalML1 "if 4 < 5 then 2 + 3 else 8 * 8 evalto 5")
  (EvalML1 "3 + if -23 < -2 * 8 then 8 else 2 + 4 evalto 11")
  (EvalML1 "3 + (if -23 < -2 * 8 then 8 else 2) + 4 evalto 15")

  ;; EvalML1Err
  (EvalML1Err "1 + true + 2 evalto error")
  (EvalML1Err "if 2 + 3 then 1 else 3 evalto error")
  (EvalML1Err "if 3 < 4 then 1 < true else 3 - false evalto error")
  
  ;; EvalML2
  (EvalML2 "x = 3, y = 2 |- x evalto 3")
  (EvalML2 "x = true, y = 4 |- if x then y + 1 else y - 1 evalto 5")
  (EvalML2 "|- let x = 1 + 2 in x * 4 evalto 12")
  (EvalML2 "|- let x = 3 * 3 in let y = 4 * x in x + y evalto 45")
  (EvalML2 "x = 3 |- let x = x * 2 in x + x evalto 12")
  (EvalML2 "|- let x = let y = 3 - 2 in y * y in let y = 4 in x + y evalto 5")

  ;; EvalML3
  (EvalML3 "|- fun x -> x + 1 evalto ()[fun x -> x + 1]")
  (EvalML3 "|- let y = 2 in fun x -> x + y evalto (y=2)[fun x -> x + y]")
  (EvalML3 "|- let sq = fun x -> x * x in sq 3 + sq 4 evalto 25")
  (EvalML3 "|- let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x) evalto 25")
  (EvalML3 "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5 evalto 5")
  (EvalML3 "|- let a = 3 in let f = fun y -> y * a in let a = 5 in f 4 evalto 12")
  (EvalML3 "|- let twice = fun f -> fun x -> f (f x) in twice (fun x -> x * x) 2 evalto 16")
  (EvalML3 "|- let twice = fun f -> fun x -> f (f x) in twice twice (fun x -> x * x) 2 evalto 65536")
  (EvalML3 "|- let compose = fun f -> fun g -> fun x -> f (g x) in 
   let p = fun x -> x * x in
   let q = fun x -> x + 4 in
   compose p q 4 
  evalto 64")
  (EvalML3 "|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k = fun x -> fun y -> x in
   s k k 7
  evalto 7")

  (EvalML3 "|- let rec fact = fun n ->
   if n < 2 then 1 else n * fact (n - 1) in
   fact 3
  evalto 6")
  (EvalML3 "|- let rec fib = fun n -> if n < 3 then 1 else fib (n - 1) + fib (n - 2) in
   fib 5
  evalto 5")
  (EvalML3 "|- let rec sum = fun f -> fun n ->
     if n < 1 then 0 else f n + sum f (n - 1) in 
   sum (fun x -> x * x) 2
  evalto 5")
  (EvalML3 "|- let fact = fun self -> fun n ->
     if n < 2 then 1 else n * self self (n - 1) in
   fact fact 3
  evalto 6")

  ;; NamelessML3 and EvalNamelessML3
  (NamelessML3     "x, y |- if x then y + 1 else y - 1 ==> if #1 then #0 + 1 else #0 - 1")
  (EvalNamelessML3 "true, 4 |- if #1 then #0 + 1 else #0 - 1 evalto 5")
  (NamelessML3     "|- let x = 3 * 3 in let y = 4 * x in x + y
==> let . = 3 * 3 in let . = 4 * #0 in #1 + #0")
  (EvalNamelessML3 "|- let . = 3 * 3 in let . = 4 * #0 in #1 + #0 evalto 45")
  (NamelessML3     "x |- let x = x * 2 in x + x ==> let . = #0 * 2 in #0 + #0")
  (EvalNamelessML3 "3 |- let . = #0 * 2 in #0 + #0 evalto 12")
  (NamelessML3     "|- let x = let y = 3 - 2 in y * y in let y = 4 in x + y
==> let . = let . = 3 - 2 in #0 * #0 in let . = 4 in #1 + #0")
  (EvalNamelessML3 "|- let . = let . = 3 - 2 in #0 * #0 in let . = 4 in #1 + #0 evalto 5")
  (NamelessML3     "|- let y = 2 in fun x -> x + y ==> let . = 2 in fun . -> #0 + #1")
  (EvalNamelessML3 "|- let . = 2 in fun . -> #0 + #1 evalto (2)[fun . -> #0 + #1]")
  (NamelessML3     "|- let sm = fun f -> f 3 + f 4 in sm (fun x -> x * x)
==> let . = fun . -> #0 3 + #0 4 in #0 (fun . -> #0 * #0)")
  (EvalNamelessML3 "|- let . = fun . -> #0 3 + #0 4 in #0 (fun . -> #0 * #0) evalto 25")
  (NamelessML3     "|- let a = 3 in let f = fun y -> y * a in let a = 5 in f 4 ==> let . = 3 in let . = fun . -> #0 * #1 in let . = 5 in #1 4")
  (EvalNamelessML3 "|- let . = 3 in let . = fun . -> #0 * #1 in let . = 5 in #1 4 evalto 12")
  (NamelessML3 "|- let rec fact = fun n ->
     if n < 2 then 1 else n * fact (n - 1) in
     fact 3
==>
   let rec . = fun . -> 
     if #0 < 2 then 1 else #0 * #1 (#0 - 1) in #0 3")
  (EvalNamelessML3 "|- let rec . = fun . -> 
     if #0 < 2 then 1 else #0 * #1 (#0 - 1) in #0 3
   evalto 6")

  ;; EvalML4
  (EvalML4 "|- (1 + 2) :: (3 + 4) :: [] evalto 3 :: 7 :: []")
  (EvalML4 "|- let f = fun x -> match x with [] -> 0 | a :: b -> a in
    f (4::[]) + f [] + f (1 :: 2 :: 3 :: [])
   evalto 5")
  (EvalML4 "|- let rec f = fun x -> if x < 1 then [] else x :: f (x - 1) in
    f 3 evalto 3 :: 2 :: 1 :: []")
  (EvalML4 "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length (1 :: 2 :: 3 :: []) evalto 3")
  (EvalML4 "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length ((1 :: 2 :: []) :: (3 :: 4 :: 5 :: []) :: []) evalto 2")
  (EvalML4 "|- let rec append = fun l1 -> fun l2 -> 
      match l1 with [] -> l2 | x :: y -> x :: append y l2 in
    append (1 :: 2 :: []) (3 :: 4 :: 5 :: []) evalto 1 :: 2 :: 3 :: 4 :: 5 :: []")
  (EvalML4 "|- let rec apply = fun l -> fun x ->
      match l with [] -> x | f :: l -> f (apply l x) in
    apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4 
   evalto 49")
  (EvalML4 "|- let rec apply = fun l -> fun x ->
      match l with [] -> x | f :: l -> apply l (f x) in
    apply ((fun x -> x * x) :: (fun y -> y + 3) :: []) 4 
   evalto 19")

  ;; EvalML5
  (EvalML5 "|- let rec max = fun l -> match l with 
       x :: [] -> x 
     | x :: y :: z -> if x < y then max (y :: z) else max (x :: z) in
   max (9 :: 2 :: 3 :: [])
  evalto 9")
  (EvalML5 "|- let rec heads = fun l -> match l with
       [] -> []
     | [] :: l' -> heads l'
     | (x :: _) :: l' -> x :: heads l' in
   heads ((1 :: 2 :: []) :: [] :: (3 :: []) :: [])
  evalto 1 :: 3 :: []")

  ;; TypingML4
  (TypingML4 "|- 3 + 5 : int")
  (TypingML4 "|- if 4 < 5 then 2 + 3 else 8 * 8 : int")
  (TypingML4 "x : bool, y : int |- if x then y + 1 else y - 1 : int")
  (TypingML4 "|- let x = 3 < 2 in let y = 5 in if x then y else 2 : int")

  ;; function types
  (TypingML4 "|- fun x -> x + 1 : int -> int")
  (TypingML4 "|- let f = fun x -> x + 1 in f 4 : int")
  (TypingML4 "|- fun f -> f 0 + f 1 : (int -> int) -> int")
  (TypingML4 "|- let max = fun x -> fun y -> if x < y then y else x in max 3 5 : int")

  ;; nil is polymorphic
  (TypingML4 "|- 4 :: [] : int list")
  (TypingML4 "|- true :: false :: [] : bool list")

  ;; polymorphic combinators
  (TypingML4 "|- fun x -> fun y -> x : int -> int -> int")
  (TypingML4 "|- fun x -> fun y -> x : bool -> int -> bool")
  (TypingML4 "|- let k = fun x -> fun y -> x in k 3 true : int")
  (TypingML4 "|- let k = fun x -> fun y -> x in k (1::[]) 3 : int list")
  (TypingML4 "|- let k = fun x -> fun y -> x in k true (fun x -> x + 1) : bool")

  (TypingML4 "|- let compose = fun f -> fun g -> fun x -> f (g x) in
   let p = fun x -> x * x in
   let q = fun x -> x + 4 in
   compose p q : int -> int")
  (TypingML4 "|- let compose = fun f -> fun g -> fun x -> f (g x) in
   let p = fun x -> if x then 3 else 4 in
   let q = fun x -> x < 4 in
   compose p q : int -> int")
  
  ;; SKK is polymorphic identity
  (TypingML4 "|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k1 = fun x -> fun y -> x in
   let k2 = fun x -> fun y -> x in
   s k1 k2 : int -> int")

  (TypingML4 "|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k1 = fun x -> fun y -> x in
   let k2 = fun x -> fun y -> x in
   s k1 k2 (fun x -> x + 1) : int -> int")

   ;; recursive functions
  (TypingML4 "|- let rec fact = fun n ->
     if n < 2 then 1 else n * fact (n - 1) in
     fact 3 : int")
  (TypingML4 "|- let rec sum = fun f -> fun n ->
     if n < 1 then 0 else f n + sum f (n - 1) in 
   sum (fun x -> x * x) 2 : int")

   ;; polymorphism in lists
   (TypingML4 "|- let l = (fun x -> x) :: (fun y -> 2) :: (fun z -> z + 3) :: [] in 2 : int")

   (TypingML4 "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length : int list -> int")

   (TypingML4 "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
    length ((fun x -> x) :: (fun y -> y + 3) :: []) : int")

   (TypingML4 "|- let rec append = fun l1 -> fun l2 -> 
     match l1 with [] -> l2 | x :: y -> x :: append y l2 in
     append : int list -> int list -> int list")

   (TypingML4 "|- let rec append = fun l1 -> fun l2 -> 
     match l1 with [] -> l2 | x :: y -> x :: append y l2 in
     append (true :: []) (false :: []) : bool list")

   (TypingML4 "|- let rec map = fun f -> fun l ->
     match l with [] -> [] | x :: y -> f x :: map f y in
     map (fun x -> x < 3) (4 :: 5 :: 1 :: []) : bool list")

   ;; PolyTypingML4 (let-polymorphism): 
   (PolyTypingML4 "|- fun x -> x : 'a -> 'a")
   (PolyTypingML4 "f: 'a.'a->'a |- f 3 : int")
   (PolyTypingML4 "f: 'a.'a->'a |- f (fun x -> x + 3) : int -> int")
   (PolyTypingML4 "|- let id = fun x -> x in id id : bool -> bool")
   (PolyTypingML4 "f: 'a 'b.'a->'b->'a |- f 3 true + f 2 4 : int")
   (PolyTypingML4 "|- let k = fun x -> fun y -> x in (k 3 true) :: (k (1::[]) 3) : int list")

   (PolyTypingML4 "|- let compose = fun f -> fun g -> fun x -> f (g x) in
   let f = fun x -> if x then 3 else 4 in
   let g = fun x -> x < 4 in
   compose f (compose g f) true : int")

   (PolyTypingML4 "|- let twice = fun f -> fun x -> f (f x) in
   twice (fun x -> x + 4) 5 : int")
   (PolyTypingML4 "|- let twice = fun f -> fun x -> f (f x) in
   twice twice (fun x -> x + 4) 5 : int")

   (PolyTypingML4 "|- let s = fun f -> fun g -> fun x -> f x (g x) in
   let k = fun x -> fun y -> x in
   s k k : 'a -> 'a")

   (PolyTypingML4 "|- let x = [] in let y = 3 :: x in true :: x : bool list")
   (PolyTypingML4 "|- let l = (fun x -> x) :: [] in
   let l1 = (fun y -> y + 1) :: l in
   (fun z -> if z then false else true) :: l : (bool -> bool) list")
   (PolyTypingML4 "|- let rec length = fun l -> match l with [] -> 0 | x :: y -> 1 + length y in
   length (3 :: 2 :: []) + length ((1 :: []) :: []) : int")

   (PolyTypingML4 "|- let rec map = fun f -> fun l ->
     match l with [] -> [] | x :: y -> f x :: map f y in
   map (fun x -> x < 3) (map (fun x -> x * 2) (4 :: 5 :: 1 :: [])) : bool list")
   (PolyTypingML4 "|- let rec map = fun f -> fun l ->
     match l with [] -> [] | x :: y -> f x :: map f y in
   let f = map (fun x -> x) in
   let a = f (3 :: []) in f (true :: []) : bool list")

   (PolyTypingML4 "|- let f = fun x -> 
             let g = fun y -> x :: [] in 
             if true then g 3 else g false in
   match f 2 with [] -> f true | x :: y -> [] : bool list")
   (PolyTypingML4 "|- let f = fun x -> 
             let g = fun y -> y x :: [] in g (fun z -> 4) in
   match f true with [] -> 3 :: [] | x :: y -> f x : int list")

   ;; EvalContML1
   (EvalContML1 "3 >> _ evalto 3")
   (EvalContML1 "5 >> {3 + _} evalto 8")
   (EvalContML1 "3 + 5 evalto 8")
   (EvalContML1 "(4 + 5) * (1 - 10) evalto -81")
   (EvalContML1 "if 4 < 5 then 2 + 3 else 8 * 8 evalto 5")
   (EvalContML1 "3 + (if -3 < -2 * 8 then 8 else 2) + 4 evalto 9")

   ;; EvalContML4
   (EvalContML4 "|- let x = 1 + 2 in x * 4 evalto 12")
   (EvalContML4 "|- let add1 = fun x -> x + 1 in add1 3 evalto 4")
   (EvalContML4 "|- let rec fact = fun n ->
   if n < 2 then 1 else n * fact (n - 1) in
   fact 3
  evalto 6")
   (EvalContML4 "k = [{3 + _} >> _ ] |- 1 + k 2 evalto 5")
   (EvalContML4 "|- 3 + (letcc k in 1 + k 2) evalto 5")
   (EvalContML4 "|- let rec fact = fun n ->
   if n < 2 then 1 else n * fact (n - 1) in
   3 + (letcc k in 1 + k 2 + fact 100) evalto 5")
   (EvalContML4 "|- let sm = fun f -> f 3 + f 4 in letcc k in sm k evalto 3")
   (EvalContML4 "|- let f = fun x -> fun k1 -> fun k2 ->
     if x < 0 then k1 x else k2 x in
   1 + (letcc k1 in 2 + letcc k2 in f (-2) k1 k2) evalto -1")
   (EvalContML4 "|- let f = fun x -> fun k1 -> fun k2 ->
     if x < 0 then k1 x else k2 x in
   1 + (letcc k1 in 2 + letcc k2 in f 2 k1 k2) evalto 5")
   (EvalContML4 "|- let rec findneg = fun l -> match l with 
       [] -> false 
     | x :: l -> if x < 0 then true else findneg l
   in findneg (1 :: 2 :: -3 :: 4 :: []) evalto true")
   (EvalContML4 "|- let findneg = fun l ->
     letcc k in
       let rec aux = fun l -> match l with 
         [] -> false 
       | x :: l -> if x < 0 then k true else aux l
     in aux l
   in findneg (1 :: 2 :: -3 :: 4 :: []) evalto true")

   ;; EvalRefML3
   (EvalRefML3 "@l = 2 / x = @l |- !x + 3 evalto 5 / @l = 2")
   (EvalRefML3 "@l = 2 / x = @l |- x := !x + 1 evalto 3 / @l = 3")
   (EvalRefML3 "|- let r = ref true in !r evalto true / @l = true")
   (EvalRefML3 "|- let incr = fun x -> x := !x + 1 in
   let x = ref 0 in
   let z = incr x in
   !x evalto 1 / @l = 1")
   (EvalRefML3 "|- let c =
     let x = ref 0 in
     fun y -> if y then x := !x + 1 else !x in
   let y = c true in
   let y = c true in
   c false evalto 2 / @l = 2")
   (EvalRefML3 "|- let newc = fun x ->
     let x = ref x in
     fun y -> if y then x := !x + 1 else !x in
   let c1 = newc 5 in
   let c2 = newc 4 in
   let y = c1 true in
   let y = c2 true in
   c1 false evalto 6 / @l1 = 6, @l2 = 5")
   (EvalRefML3 "|- let f = fun r1 -> fun r2 -> let z = r2 := 3 in !r1 in
   let r = ref 0 in f r r
  evalto 3 / @l = 3")
   (EvalRefML3 "|- let x = ref 2 in let y = ref 3 in
   let refx = ref x in let refy = ref y in
   let z = !refx := !(!refy) in !x
  evalto 3 / @l1 = 3, @l2 = 3, @l3 = @l1, @l4 = @l2")
   (EvalRefML3 "|- let f = ref (fun x -> x) in
   let fact = fun n -> if n < 1 then 1 else n * !f (n - 1) in
   let z = f := fact in
   fact 3 
  evalto 6 / @l1 = (f = @l1)[fun n -> if n < 1 then 1 else n * !f (n - 1)]")
   (EvalRefML3 "|- let rec do = fun f -> fun i ->
     if i < 1 then 0
     else let x = f i in do f (i - 1) in 
   let x = ref 0 in 
   let sum = fun i -> x := !x + i in
   let y = do sum 3 in !x 
  evalto 6 / @l = 6")
))
