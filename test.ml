open! Core_kernel
open Day_counter

(* ISDA - Example 1: End dates do not involve the last day of February *)
let start_dates = [
  Date.create_exn ~y:2006 ~m:Month.Aug ~d:20; 
  Date.create_exn ~y:2007 ~m:Month.Feb ~d:20;
  Date.create_exn ~y:2006 ~m:Month.Aug ~d:31; 
]

let end_dates = [
  Date.create_exn ~y:2007 ~m:Month.Feb ~d:20; 
  Date.create_exn ~y:2007 ~m:Month.Aug ~d:20;
  Date.create_exn ~y:2007 ~m:Month.Feb ~d:28;
]

let expected = [180; 180; 177]  

let us360 start_dates end_dates expected = 
  let start_ends = List.zip_exn start_dates end_dates in
  let start_ends_expected = List.zip_exn start_ends expected in
  List.map start_ends_expected (fun ((s,e),exp) -> fun () -> Alcotest.(check int) "Check us360" exp (US_thirty_360.day_count s e))   

let test_set = 
  List.map (us360 start_dates end_dates expected) (fun t -> "us360", `Quick, t)  

let () =
  Alcotest.run "Test day counters" [
    "US_thirty_360", test_set;      
  ]
