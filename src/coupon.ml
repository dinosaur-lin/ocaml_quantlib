open! Core_kernel

type t = {
  date: Date.t;
  amount: float;
}

let create dt amt = { 
   date = dt ;
   amount = amt;
}

let has_occured cf ref_date = 
  if Date.(cf.date >= ref_date) then true else false

let amount cf = cf.amount

let date cf = cf.date


