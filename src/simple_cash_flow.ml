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
  let r = Date.compare cf.date ref_date in 
  match r with 
  | -1 -> true
  | _ -> false
