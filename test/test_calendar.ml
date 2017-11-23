open! Core_kernel

let () = 
  (Calendar.adjust Calendar.USGovernmentBondCalendar Business_day_convention.Following (Date.create_exn ~y:2017 ~m:Month.Nov ~d:11))   
  |> Date.to_string
  |> print_endline