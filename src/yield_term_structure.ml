open Core_kernel

type jump_t = {
  ts_quote: float;
  ts_time: Time.t;
}

type jump_d = {
  ts_quote: float;
  ts_date: Date.t;
}
  
type t = {
  dc: Day_counter.t;  
  jump_times: jump_t;  
}


(* 
let discount yts time_frac =
  let disc = 0.5 (* disc_implementation *) in
  *)


(* let discount time extrapolate = *)
