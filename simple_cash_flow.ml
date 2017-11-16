open! Core_kernel

type t = {
  date: Date.t;
  amount: float;
}

let create dt amt =
  { 
   date = dt ;
   amount = amt;
  }
