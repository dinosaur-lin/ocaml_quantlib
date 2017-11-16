open! Core_kernel

type t =
  | USSettlementCalendar
  | USGovernmentBondCalendar

exception Not_implemented

(** a bit difficult to get the day of year now,, so I am not implementing easter holiday? *)
let western_easter_mondays = [|
  98;  90; 103;  95; 114; 106;  91; 111; 102;        (** 1901-1909 *)
  87; 107;  99;  83; 103;  95; 115;  99;  91; 111;   (** 1910-1919 *)
  96;  87; 107;  92; 112; 103;  95; 108; 100;  91;   (** 1920-1929 *)
  111;  96;  88; 107;  92; 112; 104;  88; 108; 100;  (** 1930-1939 *)
  85; 104;  96; 116; 101;  92; 112;  97;  89; 108;   (** 1940-1949 *)
  100;  85; 105;  96; 109; 101;  93; 112;  97;  89;  (** 1950-1959 *)
  109;  93; 113; 105;  90; 109; 101;  86; 106;  97;   (** 1960-1969 *)
  89; 102;  94; 113; 105;  90; 110; 101;  86; 106;   (** 1970-1979 *)
  98; 110; 102;  94; 114;  98;  90; 110;  95;  86;   (** 1980-1989 *)
  106;  91; 111; 102;  94; 107;  99;  90; 103;  95;   (** 1990-1999 *)
  115; 106;  91; 111; 103;  87; 107;  99;  84; 103;   (** 2000-2009 *)
  95; 115; 100;  91; 111;  96;  88; 107;  92; 112;   (** 2010-2019 *)
  104;  95; 108; 100;  92; 111;  96;  88; 108;  92;   (** 2020-2029 *)
  112; 104;  89; 108; 100;  85; 105;  96; 116; 101;   (** 2030-2039 *)
  93; 112;  97;  89; 109; 100;  85; 105;  97; 109;   (** 2040-2049 *)
  101;  93; 113;  97;  89; 109;  94; 113; 105;  90;   (** 2050-2059 *)
  110; 101;  86; 106;  98;  89; 102;  94; 114; 105;   (** 2060-2069 *)
  90; 110; 102;  86; 106;  98; 111; 102;  94; 114;   (** 2070-2079 *)
  99;  90; 110;  95;  87; 106;  91; 111; 103;  94;   (** 2080-2089 *)
  107;  99;  91; 103;  95; 115; 107;  91; 111; 103;   (** 2090-2099 *)
  88; 108; 100;  85; 105;  96; 109; 101;  93; 112;   (** 2100-2109 *)
  97;  89; 109;  93; 113; 105;  90; 109; 101;  86;   (** 2110-2119 *)
  106;  97;  89; 102;  94; 113; 105;  90; 110; 101;   (** 2120-2129 *)
  86; 106;  98; 110; 102;  94; 114;  98;  90; 110;   (** 2130-2139 *)
  95;  86; 106;  91; 111; 102;  94; 107;  99;  90;   (** 2140-2149 *)
  103;  95; 115; 106;  91; 111; 103;  87; 107;  99;   (** 2150-2159 *)
  84; 103;  95; 115; 100;  91; 111;  96;  88; 107;   (** 2160-2169 *)
  92; 112; 104;  95; 108; 100;  92; 111;  96;  88;   (** 2170-2179 *)
  108;  92; 112; 104;  89; 108; 100;  85; 105;  96;   (** 2180-2189 *)
  116; 101; 93; 112; 97; 89; 109; 100; 85; 105 (** 2190-2199 *)
|]
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
  | USSettlementCalendar ->
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
    if h then false else true
  | USGovernmentBondCalendar -> 
    (* TODO: easter Monday is missing*)
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
    if h then false else true 

let is_holiday t dt =
  not (is_business_day t dt)

let rec find_business_day t dt next_day =
  if is_business_day t dt then dt else 
    let dt1 = next_day dt in
    find_business_day t dt1 next_day

let adjust t c dt =
  match c with 
  | Business_day_convention.Unadjusted -> dt
  | Business_day_convention.Following -> 
    let next_day = fun dt -> Date.add_days dt 1 in 
    find_business_day t dt next_day
  | Business_day_convention.Preceding -> 
    let next_day = fun dt -> Date.add_days dt (-1) in 
    find_business_day t dt next_day
  | _ -> raise Not_implemented (* not implemented exception *)
