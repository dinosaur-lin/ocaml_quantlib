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
  | Biweekly
  | Weekly
  | Daily
  | OtherFrequency

val to_int: t -> int
