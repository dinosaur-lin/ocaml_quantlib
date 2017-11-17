type t = {
  rate: float;
  comp: Compounding.t; 
  freq: Frequency.t; 
}

val compound_factor: t -> float -> float