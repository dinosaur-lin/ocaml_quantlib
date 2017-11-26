open! Core_kernel

type dc_t =
  | Thirty360
  | Actual360
  | Actual365  
  | Simple
  | ActualActual

type t = {
  name: string;
  dc_type: dc_t;
  date_adjust: Date.t * Date.t -> Date.t * Date.t;
  day_count: Date.t -> Date.t -> int;
  year_frac: (t -> Date.t -> Date.t -> int) -> t -> Date.t -> Date.t -> float;
}

val create_US_thirty_360: unit -> t

val day_count: t -> Date.t -> Date.t -> int

val year_frac: t -> Date.t -> Date.t -> float