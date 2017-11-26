open! Core_kernel

type t = {
  name: string;  
  date_adjust: Date.t * Date.t -> Date.t * Date.t;
  day_count: Date.t -> Date.t -> int;
  year_frac: (t -> Date.t -> Date.t -> int) -> t -> Date.t -> Date.t -> float;
}

val create_US_thirty_360: unit -> t

val create_EU_thirty_360: unit -> t

val create_IT_thirty_360: unit -> t

val name: t -> string

val day_count: t -> Date.t -> Date.t -> int

val year_frac: t -> Date.t -> Date.t -> float