open! Core_kernel

let () = 
  (Business_calendar.adjust Business_calendar.USGovernmentBondCalendar Business_day_convention.Unadjusted (Date.today Time.Zone.utc)) 
  |> Date.to_string
  |> print_endline