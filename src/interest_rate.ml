open! Core_kernel

type t = {
  rate: float;
  comp: Compounding.t;  
  freq: Frequency.t;
  dc: Day_counter.t;
}

let compound_rate r t f =
  (1.0 +. r /. f) ** (f *. t)

let simple_rate r t =
  1.0 +. r *. t

let compound_factor ir t =
  let f = Frequency.to_int ir.freq |> float_of_int in
  match ir.comp with 
  | Compounding.Simple -> simple_rate ir.rate t
  | Compounding.Compounded -> compound_rate ir.rate t f
  | Compounding.Continous -> exp (ir.rate *. t)
  | Compounding.SimpleThenCompounded -> if t <= 1.0 /. f then simple_rate ir.rate t else compound_rate ir.rate t f
  | Compounding.CompoundedThenSimple -> if t > 1.0 /. f then simple_rate ir.rate t else compound_rate ir.rate t f

let discount_factor ir t =
  1.0 /. compound_factor ir t 

let discount_factor ir d1 d2 =
  let time = Day_counter.year_frac ir.dc d1 d2 in
  discount_factor ir time


