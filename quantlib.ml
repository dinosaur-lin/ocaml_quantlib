open Core_kernel
open Day_counter

let () =
    let test_start_date = Date.create_exn ~y:2006 ~m:Month.Aug ~d:20 in 
    let test_end_date = Date.create_exn ~y:2007 ~m:Month.Feb ~d:20 in 
    let daycount = US_thirty_360.day_count test_start_date test_end_date in
    let yearfrac = US_thirty_360.year_fraction test_start_date test_end_date in
    let _ = daycount |> print_int in
    let _ = print_newline () in
    let _ = yearfrac |> print_float in
    print_newline ()
    