open! Core_kernel

type t =
  | USSettlementCalendar
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

let extract_day_month_year_weekday dt =
  let d = Date.day dt in
  let m = Date.month dt in
  let y = Date.year dt in
  let w = Date.day_of_week dt in
  (d, m, y, w)

let is_day_in_nth_week n d =
  let start_d = (n - 1)*7+1 in
  let end_d = n * 7 in 
  d >= start_d && d <= end_d

let is_washington_birthday dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  if y >= 1971 then (is_day_in_nth_week 3 d) && w = Day_of_week.Mon && m = Month.Feb
  else dt = ((Date.create_exn ~y:y ~m:Month.Feb ~d:22) |> adjust_weekend_holiday_US)

let is_memorial_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  if y >= 1971 then d >=25 && w = Day_of_week.Mon && m = Month.Feb (* last *)
  else dt = ((Date.create_exn ~y:y ~m:Month.May ~d:30) |> adjust_weekend_holiday_US)

let is_labor_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  is_day_in_nth_week 1 d && w = Day_of_week.Mon && m = Month.Sep

let is_columbus_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  is_day_in_nth_week 2 d && w = Day_of_week.Mon && m = Month.Oct && y >= 1971

let is_veterans_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  if y <= 1970 || y >= 1978 then dt = ((Date.create_exn ~y:y ~m:Month.Nov ~d:11) |> adjust_weekend_holiday_US)
  else is_day_in_nth_week 4 d && w = Day_of_week.Mon && m = Month.Oct
  
let is_new_year_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  dt = ((Date.create_exn ~y:y ~m:Month.Jan ~d:1) |> adjust_weekend_holiday_US)

let is_martin_luther_king_birthday dt = 
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  (is_day_in_nth_week 3 d) && w = Day_of_week.Mon && m = Month.Jan && y >= 1983

let is_thanksgiving_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  is_day_in_nth_week 4 d && w = Day_of_week.Thu && m = Month.Nov

let is_independence_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  dt = ((Date.create_exn ~y:y ~m:Month.Jul ~d:4) |> adjust_weekend_holiday_US)

let is_chrismas_day dt =
  let (d, m, y, w) = extract_day_month_year_weekday dt in
  dt = ((Date.create_exn ~y:y ~m:Month.Dec ~d:25) |> adjust_weekend_holiday_US)

let is_business_day t dt =
  match t with 
  | USGovernmentBondCalendar ->
    let h = Date.is_weekend dt 
      || is_new_year_day dt 
      || is_martin_luther_king_birthday dt 
      || is_washington_birthday dt
      || is_memorial_day dt 
      || is_labor_day dt 
      || is_independence_day dt
      || is_labor_day dt
      || is_columbus_day dt
      || is_veterans_day dt
      || is_thanksgiving_day dt
      || is_chrismas_day dt in
    if Date.is_weekend dt || h then false else true
   | USSettlementCalendar -> true 

let adjust t c dt =
  match c with 
  | Business_day_convention.Unadjusted -> dt
  | Business_day_convention.Following -> dt
  | _ -> dt (* not implemented exception *)
