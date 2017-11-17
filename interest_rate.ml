type t = {
  rate: float;
  comp: Compounding.t;  
  freq: Frequency.t;
}


let compound_factor ir year_frac =
  let f = Frequency.to_int ir.freq in
  match ir.comp with 
  | Compounding.Simple -> 1.0 +. ir.rate *. year_frac
  | Compounding.Compounded -> (1.0 + ir.rate /. f) ** f *. year_frac  
