open OUnit2
open Tiger

let sample_programs = Programs.sample_programs

let test_parsing =
  (* parse_test just ensures there's no exception thrown, matching `parsetest.sml` *)
  let parse_test input _ = ignore @@ Parser.parse_string input in
  let gen_parse_test (test_name, test_input) =
    test_name >:: parse_test test_input
  in
  "parsing" >::: List.map gen_parse_test sample_programs

let () = run_test_tt_main ("suite" >::: [ test_parsing ])
