open! Core_kernel

type convention = 
  | USThirty360
  | BondThirty360
  | EuroThirty360
  | EuroBondThirty360
  | ItalianThirty360
  | Simple

val day_count: convention -> Date.t -> Date.t -> int