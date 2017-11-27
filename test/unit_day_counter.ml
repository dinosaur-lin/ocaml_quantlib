open! Core_kernel
open Ocaml_quantlib

type test_entry = {
  start_date: Date.t;
  end_date: Date.t;
  expected_dc: int;
}

let us_thirty_360_dc = Day_counter.create_US_thirty_360 ()

let eu_thrity_360_dc = Day_counter.create_EU_thirty_360 ()

let test_dcs = Map.of_alist_exn [
    (Day_counter.name us_thirty_360_dc,us_thirty_360_dc);
    (Day_counter.name eu_thrity_360_dc,eu_thrity_360_dc);
  ] ~comparator:(String.comparator)

let test_entries = Map.of_alist_exn [
    (* ISDA - Example 1: End dates do not involve the last day of February *)
    (Day_counter.name us_thirty_360_dc, [
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
      ]);
    (Day_counter.name eu_thrity_360_dc, [
        {
          start_date = Date.create_exn ~y:2006 ~m:Month.Feb ~d:28;
          end_date = Date.create_exn ~y:2006 ~m:Month.Aug ~d:31;
          expected_dc = 182;
        };
    ])        
  ] ~comparator:(String.comparator)

let test_cases_for_dc dc test_entries = 
  let dc_name = Day_counter.name dc in
    List.map test_entries (fun e -> 
      dc_name, fun () -> Alcotest.(check int) dc_name e.expected_dc (Day_counter.day_count dc e.start_date e.end_date))

let all_test_cases dc_map test_entries_map =
  let dc_names = Map.keys dc_map in
  let test_cases_list = List.map dc_names (fun n -> (test_cases_for_dc (Map.find_exn dc_map n) (Map.find_exn test_entries_map n))) in
  List.fold_left test_cases_list ~init:[] ~f:(fun b a -> b @ a)

let test_set = 
  List.map (all_test_cases test_dcs test_entries) (fun (n, t) -> n, `Quick, t) 
