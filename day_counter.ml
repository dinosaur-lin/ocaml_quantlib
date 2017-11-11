open Core_kernel

module type Daycount = 
sig    
    val day_count: Date.t -> Date.t -> int
end

module Daycount_utils = struct
    let extract_day_month_year d1 d2 = (
            Date.year d1,
            Date.year d2,
            Date.month d1 |> Month.to_int,
            Date.month d2 |> Month.to_int,
            Date.day d1,
            Date.day d2
        )    

    let thirdy_360_day_count y1 y2 m1 m2 d1 d2 =
         360 * (y2 - y1) + 30 * (m2 - m1 - 1) + (max 0 30 - d1) + (min 30 d2)
end

module USA_daycount: Daycount = struct     
    let day_count d1 d2 = let (yy1,yy2,mm1,mm2,dd1,dd2) = Daycount_utils.extract_day_month_year d1 d2 in                          
                          let (ddd2,mmm2) = if dd2 = 31 && dd1 < 30 then (1, mm2+1) else (dd2, mm2) in
                          Daycount_utils.thirdy_360_day_count yy1 yy2 mm1 mmm2 dd1 ddd2
end

module EU_daycount: Daycount = struct 
    let day_count d1 d2 = let (yy1,yy2,mm1,mm2,dd1,dd2) = Daycount_utils.extract_day_month_year d1 d2 in                           
                          Daycount_utils.thirdy_360_day_count yy1 yy2 mm1 mm2 dd1 dd2
end

module IT_daycount: Daycount = struct 
    let modify_day m d = if m = 2 && d > 27 then 30 else d
    let day_count d1 d2 = let (yy1,yy2,mm1,mm2,dd1,dd2) = Daycount_utils.extract_day_month_year d1 d2 in
                          let ddd1 = modify_day mm1 dd1 in
                          let ddd2 = modify_day mm2 dd2 in
                          Daycount_utils.thirdy_360_day_count yy1 yy2 mm1 mm2 ddd1 ddd2
end

module type Daycounter = sig
    include Daycount
    val year_fraction: Date.t -> Date.t -> float
end

module Thirty_360(D: Daycount): Daycounter = struct
    let year_fraction d1 d2 = (float_of_int (D.day_count d1 d2)) /. 360.0
    let day_count = D.day_count
end

module US_thirty_360 = Thirty_360(USA_daycount)
module EU_thirty_360 = Thirty_360(EU_daycount)
module IT_thirty_360 = Thirty_360(IT_daycount)