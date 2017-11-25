open! Core_kernel

type t = 
  | USThirty360
  | BondThirty360
  | EuroThirty360
  | EuroBondThirty360
  | ItalianThirty360
  | Simple

val day_count: t -> Date.t -> Date.t -> int

val year_frac: t -> Date.t -> Date.t -> float