open! Core_kernel

module Utils = struct

  let extract_day_month_year d = (
    Date.year d,            
    Date.month d |> Month.to_int,            
    Date.day d            
  )

  let thirdy_360_day_count y1 y2 m1 m2 d1 d2 =
    360 * (y2 - y1) + 30 * (m2 - m1 - 1) + (max 0 (30 - d1)) + (min 30 d2)
end

type t = {
  name: string;  
  date_adjust: Date.t * Date.t -> Date.t * Date.t;
  day_count: Date.t -> Date.t -> int;
  year_frac: (t -> Date.t -> Date.t -> int) -> t -> Date.t -> Date.t -> float;
}

let name t = t.name

let day_count t d1 d2 = 
  let dd1,dd2 = t.date_adjust (d1,d2) in
  t.day_count dd1 dd2
  
let year_frac t d1 d2 =
  t.year_frac day_count t d1 d2

let thirty_360_day_count d1 d2 =
    360 * ((Date.year d2) - (Date.year d1)) 
      + 30 * ((Date.month d2 |> Month.to_int) - (Date.month d1 |> Month.to_int) - 1) 
      + (max 0 (30 - (Date.day d1))) 
      + (min 30 (Date.day d2))

let us_date_adjust (d1,d2) =
  let (yy1,mm1,dd1) = Utils.extract_day_month_year d1 in
  let (yy2,mm2,dd2) = Utils.extract_day_month_year d2 in
  let (ddd2,mmm2) = if dd2 = 31 && dd1 < 30 then (1, mm2+1) else (dd2, mm2) in
  d1,(Date.create_exn ~y:yy2 ~m:(Month.of_int_exn mmm2) ~d:ddd2)

let it_date_adjust (d1,d2) =
  let modify_day m d = if m = 2 && d > 27 then 30 else d in
  let (yy1,mm1,dd1) = Utils.extract_day_month_year d1 in
  let (yy2,mm2,dd2) = Utils.extract_day_month_year d2 in
  let ddd1 = modify_day mm1 dd1 in
  let ddd2 = modify_day mm2 dd2 in
  (Date.create_exn ~y:yy1 ~m:(Month.of_int_exn mm1) ~d:ddd1),(Date.create_exn ~y:yy2 ~m:(Month.of_int_exn mm2) ~d:ddd2)    

let thirty_360_year_frac dc_func t d1 d2 =
  ((dc_func t d1 d2) |> float_of_int) /. 360.0
  
let create_US_thirty_360 () =
  {
    name = "30/360 (Bond Basis)";    
    day_count = thirty_360_day_count; 
    date_adjust = us_date_adjust;
    year_frac = thirty_360_year_frac;   
  }

let create_EU_thirty_360 () =
  {
    name = "30E/360 (Eurobond Basis)";    
    day_count = thirty_360_day_count; 
    date_adjust = (fun (d1,d2) -> d1,d2);
    year_frac = thirty_360_year_frac;   
  }

let create_IT_thirty_360 () =
  {
    name = "30/360 (Italian)";        
    date_adjust = it_date_adjust;
    day_count = thirty_360_day_count; 
    year_frac = thirty_360_year_frac;   
  }

let is_end_of_month dt =
  let m = Date.month dt in 
  let d = Date.day dt in
  match m with 
  | Month.Jan -> d = 31
  | Month.Feb -> if Date.is_leap_year ~year:(Date.year dt) then d = 29 else d = 28
  | Month.Mar -> d = 31
  | Month.Apr -> d = 30
  | Month.May -> d = 31
  | Month.Jun -> d = 30
  | Month.Jul -> d = 31
  | Month.Aug -> d = 31
  | Month.Sep -> d = 30
  | Month.Oct -> d = 31
  | Month.Nov -> d = 30
  | Month.Dec -> d = 31

let month_int dt =
  Date.month dt |> Month.to_int

let simple_year_fract fallback dc_func t d1 d2 =
  let dd1 = Date.day d1 in
  let dd2 = Date.day d2 in  
  if dd1 = dd2 || 
  (* e.g., Aug 30 -> Feb 28 ?*)
    (dd1 > dd2 && is_end_of_month d2) ||
    (* Feb 28 -> Aug 30 *)
    (dd1 < dd2 && is_end_of_month d1)
    then (((Date.year d2) - (Date.year d1)) |> float_of_int) +.
      (((month_int d2) - (month_int d1)) |> float_of_int) /. 12.0
    else year_frac fallback d1 d2  

let create_simple ?(fallback=(create_US_thirty_360 ())) () = 
  {
    name = "Simple";
    date_adjust = fallback.date_adjust;
    day_count = fallback.day_count;
    year_frac = simple_year_fract fallback;
  }


