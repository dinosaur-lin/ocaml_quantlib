let () = begin
  Alcotest.run "ocaml_quantlib" [    
    "day counter", Unit_day_counter.test_set;
  ]
end