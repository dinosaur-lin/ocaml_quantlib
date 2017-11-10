open Core_kernel

module type Daycount = 
sig    
    val day_count: Date.t -> Date.t -> int
end

module USA_daycount: Daycount = struct 
    let day_count d1 d2 = let dd1 = Date.day d1 in
                          let dd2 = Date.day d2 in
                          let mm1 = Date.month d1 |> Month.to_int in
                          let mm2 = Date.month d2 |> Month.to_int in
                          let yy1 = Date.year d1 in 
                          let yy2 = Date.year d2 in 
                          let (ddd2,mmm2) = if dd2 = 31 && dd1 < 30 then (1, mm2+1) else (dd2, mm2) in
                          360*(yy2-yy1)+30*(mmm2-mm1-1)+(max 0 30-dd1)+(min 30 ddd2)
end

module EU_daycount: Daycount = struct 
    let day_count d1 d2 = let dd1 = Date.day d1 in
                          let dd2 = Date.day d2 in
                          let mm1 = Date.month d1 |> Month.to_int in
                          let mm2 = Date.month d2 |> Month.to_int in
                          let yy1 = Date.year d1 in 
                          let yy2 = Date.year d2 in                           
                          360*(yy2-yy1)+30*(mm2-mm1-1)+(max 0 30-dd1)+(min 30 dd2)
end

module IT_daycount: Daycount = struct 
    let modify_day m d = if m = 2 && d > 27 then 30 else d
    let day_count d1 d2 = let dd1 = Date.day d1 in
                          let dd2 = Date.day d2 in
                          let mm1 = Date.month d1 |> Month.to_int in
                          let mm2 = Date.month d2 |> Month.to_int in 
                          let ddd1 = modify_day mm1 dd1 in
                          let ddd2 = modify_day mm2 dd2 in                                                    
                          let yy1 = Date.year d1 in 
                          let yy2 = Date.year d2 in                           
                          360*(yy2-yy1)+30*(mm2-mm1-1)+(max 0 30-ddd1)+(min 30 ddd2)
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

