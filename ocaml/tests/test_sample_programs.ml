open Base
open OUnit2
open Tiger

let sample_programs = Programs.sample_programs

type program_test = string * string

and 'a stage_tests = {
  runner : string -> 'a;
  fail : program_test list;
  pass : program_test list;
}

let test_stage ?after ~failing runner =
  let to_test =
    Option.value_map after ~default:sample_programs ~f:(fun f -> f.pass)
  in
  let split_assoc mems =
    List.fold ~init:([], []) ~f:(fun (l, r) ((test_name, _) as t) ->
        if List.mem mems test_name ~equal:String.equal then (t :: l, r)
        else (l, t :: r))
  in
  let fail, pass = split_assoc failing to_test in
  { runner; fail; pass }

let parsing = test_stage ~failing:[ "test49" ] Parser.parse_string

let mk_suite name stage =
  let failing = stage.fail in
  let passing = stage.pass in
  let runner = stage.runner in
  let failing_runner test =
    fst test >:: fun _ ->
    assert_bool "Expected test to fail"
    @@ Exn.does_raise (fun () -> ignore @@ runner @@ snd test)
  in
  let passing_runner test =
    fst test >:: fun _ -> ignore @@ runner @@ snd test
  in
  [
    name ^ " (pass)" >::: List.map ~f:passing_runner passing;
    name ^ " (fail)" >::: List.map ~f:failing_runner failing;
  ]

let () =
  run_test_tt_main ("suite" >::: List.concat [ mk_suite "parsing" parsing ])
