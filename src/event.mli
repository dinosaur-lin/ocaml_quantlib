open Core_kernel

type event_t = 
  | Settle
  | Maturity
  | Start
  | End

type t = {
  e: event_t;
  d: Date.t;
}

val to_date: t -> Date.t

val from_date: Date.t -> event_t -> t