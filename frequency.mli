type t = 
  | NoFrequency
  | Once
  | Annual
  | Semiannual
  | EveryFourthMonth
  | Quarterly
  | Bimonthly
  | Monthly
  | EveryFourthWeek
  | Weekly
  | Daily
  | OtherFrequency

val to_int: t -> int
