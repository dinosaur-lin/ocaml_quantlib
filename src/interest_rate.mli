open! Core_kernel

type t = {
  rate: float;
  comp: Compounding.t;  
  freq: Frequency.t;
  dc: Day_counter.t;
}

val compound_factor: t -> float -> float

val discount_factor: t -> float -> float

val discount_factor: t -> Date.t -> Date.t -> float