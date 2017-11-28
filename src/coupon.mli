open! Core_kernel

type t = {
  date: Date.t;
  amount: float;
}

val create: Date.t -> float -> t  
val has_occured: t -> Date.t -> bool
val date: t -> Date.t
val amount: t -> float