open! Core_kernel
open Ocaml_quantlib

type test_entry = {
  start_date: Date.t;
  end_date: Date.t;
  expected_dc: int;
}

let test_enties = [
  (* ISDA - Example 1: End dates do not involve the last day of February *)
  {
    start_date = Date.create_exn ~y:2006 ~m:Month.Aug ~d:20;
    end_date = Date.create_exn ~y:2007 ~m:Month.Feb ~d:20;
    expected_dc = 180;
  };
  {
    start_date = Date.create_exn ~y:2007 ~m:Month.Feb ~d:20;
    end_date = Date.create_exn ~y:2007 ~m:Month.Aug ~d:20;
    expected_dc = 180;
  };  
  {
    start_date = Date.create_exn ~y:2007 ~m:Month.Aug ~d:20;
    end_date = Date.create_exn ~y:2008 ~m:Month.Feb ~d:20;
    expected_dc = 180;
  };

  (* ISDA - Example 2: End dates include some end-February dates *)
  {
    start_date = Date.create_exn ~y:2006 ~m:Month.Aug ~d:31;
    end_date = Date.create_exn ~y:2007 ~m:Month.Feb ~d:28;
    expected_dc = 178;
  };      
]

let us_thirty_360_dc = Day_counter.create_US_thirty_360 ()

let expected = [180; 180; 180; 178]  

let test_cases dc test_entries = 
  List.map test_entries (fun e -> 
  fun () -> Alcotest.(check int) (Day_counter.name dc) e.expected_dc (Day_counter.day_count dc e.start_date e.end_date))   

let test_set = 
  List.map (test_cases us_thirty_360_dc test_enties) (fun t -> "us360", `Slow,   t)  
