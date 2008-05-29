#! /bin/sh

for i in 4 5 6
do
    ./checker -game ML$i -full -prove '|- let double = fun x -> x + x in double 4 evalto ?' | ./checker -game ML$i
done

for i in 4 5 6
do
    ./checker -game ML$i -full -prove '|- let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1) in fact 4  evalto ?' | ./checker -game ML$i
done

for i in 5 6
do
    ./checker -game ML$i -full -prove \
	'|- let rec append = fun l1 -> fun l2 -> match l1 with [] -> l2 | x::l1 -> x :: append l1 l2 in append (1 :: 2 :: []) (3::4::5::[]) evalto ?' | ./checker -game ML$i

    ./checker -game ML$i -full -prove \
	"|- let rec map = fun f -> fun l -> match l with [] -> [] | x::l' -> f x :: map f l' in map (fun x -> x + 1) (1 :: 3 :: 5 :: []) evalto ?" | ./checker -game ML$i
done

for i in 6
do
    ./checker -game ML$i -full -prove \
	'|- match (1 :: []) :: 2 :: [] with [] -> 1 | x :: y :: [] -> match x with x1 :: x2 -> x1 + y evalto ?' | ./checker -game ML$i
done
