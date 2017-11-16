open! Core_kernel

type zcb_t = {
  issue_date: Date.t;
  maturity_date: Date.t;
  payment_convention: Business_day_convention.t;
  face_amount: float;
  settlement_day: int;
  redemption: float;
  calendar: Calendar.t;
}


let redemption_date zcb = 
  Calendar.adjust zcb.calendar zcb.payment_convention zcb.maturity_date

let cash_flow zcb =
  Simple_cash_flow.create (redemption_date zcb) zcb.redemption


