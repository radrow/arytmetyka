(*Radosław Rowicki 386088*)
(* Written as task for MIMUW subject: Introduction to Functional Programming*)

module type ARYTMETYKA = sig
  (** Main type. In order: left bound, right bound, is straight *)
  type wartosc

  (* Implicite zakładamy, że wszystkie argumenty typu float są liczbami *)
  (* rzeczywistymi, tzn. są różne od infinity, neg_infinity i nan.      *)


  (* wartosc_dokladnosc x p = x +/- p% *)
  (* war.pocz.: p > 0                  *)
  (** Value with aproximation error *)
  val wartosc_dokladnosc: float -> float -> wartosc

  (* wartosc_od_do x y = [x;y]         *)
  (* war.pocz.: x <= y                 *)
  (** Value from-to *)
  val wartosc_od_do: float -> float -> wartosc

  (* wartosc_dokladna x = [x;x]        *)
  (** Exact value *)
  val wartosc_dokladna: float -> wartosc

  (* in_wartosc w x = x \in w *)
  (** Checks if x may be result of given value *)
  val in_wartosc: wartosc -> float -> bool

  (* min_wartosc w = najmniejsza możliwa wartość w,   *)
  (* lub neg_infinity jeśli brak dolnego ograniczenia.*)
  (** Get minimal matching value *)
  val min_wartosc: wartosc -> float

  (* max_wartosc w = największa możliwa wartość w,    *)
  (* lub infinity jeśli brak górnego ograniczenia.    *)
  (** Get maximal matching value *)
  val max_wartosc: wartosc -> float

  (* środek przedziału od min_wartosc do max_wartosc, *)
  (* lub nan jeśli min i max_wartosc nie są określone.*)
  (** Get average value, if possible *)
  val sr_wartosc:  wartosc -> float

  (* Operacje arytmetyczne na niedokładnych wartościach. *)
  val plus:      wartosc -> wartosc -> wartosc
  val minus:     wartosc -> wartosc -> wartosc
(** Multiplication *)
  val razy:      wartosc -> wartosc -> wartosc
(** Division *)
  val podzielic: wartosc -> wartosc -> wartosc
end;;
