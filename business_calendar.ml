open! Core_kernel

type calendar =
  | USGovernmentBondCalendar

(**
In the United States, if a holiday falls on Saturday, it's observed on the preceding Friday.
If it falls on Sunday, it's observed on the next Monday.
 *)  
let adjust_weekend_holiday_US dt =
  let d = Date.day_of_week dt in
  match d with 
  | Day_of_week.Sat -> Date.add_days dt (-1)
  | Day_of_week.Sun -> Date.add_days dt 1 
  | _ -> dt  