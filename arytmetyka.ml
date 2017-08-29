module Arytmetyka = struct
  type wartosc = float * float * bool (*left bound, right bound, is straight*)

  (** Does some typical fixes*)
  let autofix t =
    let isNan a = compare a nan = 0 in
    match t with
    | (a, b, _) when isNan a || isNan b -> (nan, nan, true)
    | (a,b, c) when a >= b && not c -> (neg_infinity, infinity, true)
    | _ -> t

  let wartosc_dokladnosc x p =
    let fix (a,b,c) = (min a b, max a b, c)
    in fix (x *. (100. -. p)/. 100., x *. (100. +. p)/. 100., true)

  let wartosc_od_do x y = (x, y, true)

  let wartosc_dokladna x = (x, x, true)

  let in_wartosc (a,b,s) x = match s with
    | true -> x >= a && x <= b
    | false -> x <= a || x >= b

  let min_wartosc (a, b, s) = if s then a else neg_infinity

  let max_wartosc (a, b, s) = if s then b else infinity

  let sr_wartosc x = match x with
    | (_, _, false) -> nan
    | (a, b, _) when a <> a || b <> b -> nan
    | (a, b, _) -> ((a +. b)/.  2.)

  let plus (a1, b1, s1) (a2, b2, s2) =
    let fixNotStraight (x, y) = if (x <= y) then (x, y, false) else (neg_infinity, infinity, true)
    in autofix (
      match (s1, s2) with
      | true, true -> (a1 +. a2, b1 +. b2, true)
      | true, false -> fixNotStraight (a2 +. b1, b2 +. a1)
      | false, true -> fixNotStraight (a1 +. b2, b1 +. a2)
      | false, false -> (neg_infinity, infinity, true)
      )

let minus f (a, b, x) = plus f (-.b, -.a, x)

let rec razy (a, b, s1) (c, d, s2) = if c = 0. && d = 0. then 0., 0., true else
    match (s1,s2) with
    | true, true ->
      let mymin x y = if y <> y (* eliminacja nanów*)
        then x
        else min x y
      and mymax x y = if y <> y
        then x
        else max x y
      in List.fold_left mymin infinity [a*.c; a*.d; b*.c; b*.d], List.fold_left mymax neg_infinity [a*.c; a*.d; b*.c; b*.d], true
    | false, false ->
      if b <= 0. || d <= 0. || a >= 0. || c >= 0. then neg_infinity, infinity, true
      else autofix (max (a*.d) (b*.c), min (a*.c) (b*.d), false)
    | true, false -> autofix (
        if a > 0. && b > 0. then a *. c, a *. d, false
        else if a < 0. && b < 0. then b *. d, b *. c, false
        else if a = 0. && b = 0. then 0., 0., true
        else neg_infinity, infinity, true
      )
    | false, true -> razy (c,d,s2) (a,b,s1)

let podzielic a b =
  let rev (a,b,s) = autofix (
      if a = neg_infinity && b = infinity then neg_infinity, infinity, true
      else if a < 0. && b = infinity then 1. /. a, 0., false
      else if a = neg_infinity && b > 0. then 0., 1. /. b, false
      else if (a > 0. && b > 0.) || (a < 0. && b < 0.) then 1. /. b, 1. /. a, s
      else if a = 0. && b > 0. then 1. /. b, infinity, true
      else if a < 0. && b = 0. then neg_infinity, 1. /. a, true
      else if a = 0. && b = 0. then neg_infinity, infinity, true
      else 1. /. a, 1. /. b, not s
    )
  in razy a (rev b)

end;;
