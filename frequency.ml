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

let to_int freq =
  match freq with 
  | NoFrequency -> -1
  | Once -> 0
  | Annual -> 1
  | Semiannual -> 2
  | EveryFourthMonth -> 3
  | Quarterly -> 4
  | Bimonthly -> 6
  | Monthly -> 12
  | EveryFourthWeek -> 13
  | Biweekly -> 26
  | Weekly -> 52
  | Daily -> 365
  | OtherFrequency -> 999