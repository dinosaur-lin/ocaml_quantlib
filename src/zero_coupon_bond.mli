open! Core_kernel

type zcb_t = {
  issue_date: Date.t;
  maturity_date: Date.t;
  payment_convention: Business_day_convention.t;
  face_amount: float;
  redemption: float;
  settlement_day: int;
  calendar: Calendar.t;
}

val redemption_date: zcb_t -> Date.t 