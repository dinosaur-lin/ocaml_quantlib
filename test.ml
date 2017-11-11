open Core_kernel
open Day_counter

module To_test = struct
  let test_us360 = let test_start_date = Date.create_exn ~y:2006 ~m:Month.Aug ~d:20 in 
    let test_end_date = Date.create_exn ~y:2007 ~m:Month.Feb ~d:20 in 
    US_thirty_360.day_count test_start_date test_end_date
end  
(* The tests *)
let us360 () =
  Alcotest.(check int) "Check us360" 180 To_test.test_us360

let test_set = [
  "\xF0\x9F\x90\xAB  Capitalize", `Quick, us360;  
]  

(* Run it *)
let () =
  Alcotest.run "My first test" [
    "test_1", test_set;    
]