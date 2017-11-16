open! Core_kernel

type t = {
  notionals: float array;
  notional_schedule: Date.t array;
  issue_date: Date.t;
  maturity_date: Date.t;
}

