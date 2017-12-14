open! Core_kernel

type event_t = 
  | Settle
  | Maturity
  | Start
  | End

type t = {
  e: event_t;
  d: Date.t;
}

let to_date evt =
  evt.d

let from_date dt ev_t = {
    e = ev_t;
    d = dt;
  }