open! Core_kernel

type convention = 
  | USThirty360
  | BondThirty360
  | EuroThirty360
  | EuroBondThirty360
  | ItalianThirty360
  | Simple

  module Utils = struct
    let extract_day_month_year d = (
            Date.year d,            
            Date.month d |> Month.to_int,            
            Date.day d            
        )

    let thirdy_360_day_count y1 y2 m1 m2 d1 d2 =
         360 * (y2 - y1) + 30 * (m2 - m1 - 1) + (max 0 (30 - d1)) + (min 30 d2)
  end

  let thirty_360_US_day_count d1 d2 = 
    let (yy1,mm1,dd1) = Utils.extract_day_month_year d1 in
    let (yy2,mm2,dd2) = Utils.extract_day_month_year d2 in
    let (ddd2,mmm2) = if dd2 = 31 && dd1 < 30 then (1, mm2+1) else (dd2, mm2) in
    Utils.thirdy_360_day_count yy1 yy2 mm1 mmm2 dd1 ddd2

  let thirty_360_EU_day_count d1 d2 =
    let (yy1,mm1,dd1) = Utils.extract_day_month_year d1 in
    let (yy2,mm2,dd2) = Utils.extract_day_month_year d2 in
    Utils.thirdy_360_day_count yy1 yy2 mm1 mm2 dd1 dd2  

  let modify_day m d = if m = 2 && d > 27 then 30 else d

  let thirty_360_IT_day_count d1 d2 =
    let (yy1,mm1,dd1) = Utils.extract_day_month_year d1 in
    let (yy2,mm2,dd2) = Utils.extract_day_month_year d2 in
    let ddd1 = modify_day mm1 dd1 in
    let ddd2 = modify_day mm2 dd2 in
    Utils.thirdy_360_day_count yy1 yy2 mm1 mm2 ddd1 ddd2    

  let day_count c d1 d2 = match c with 
    | USThirty360 | BondThirty360 -> thirty_360_US_day_count d1 d2 
    | EuroThirty360 | EuroBondThirty360 -> thirty_360_EU_day_count d1 d2
    | ItalianThirty360 -> thirty_360_IT_day_count d1 d2
    | Simple ->  thirty_360_US_day_count d1 d2       