module ErrorMsg : sig
  val any_errors : bool ref
  val file_name : string ref
  val line_num : int ref
  val line_pos : int list ref
  val source_stream : in_channel ref
  val error : int -> string -> unit
  val reset : unit -> unit

  exception Impossible

  val impossible : string -> 'a (* raises Impossible *)
end = struct
  let any_errors = ref false
  let file_name = ref ""
  let line_num = ref 1
  let line_pos = ref [ 1 ]
  let source_stream = ref stdin

  let reset () =
    any_errors := false;
    file_name := "";
    line_num := 1;
    line_pos := [ 1 ];
    source_stream := stdin

  let rec look pos = function
    | a :: rest, n ->
        if a < pos then
          List.iter print_string
            [ ":"; Int.to_string n; "."; pos - a |> Int.to_string ]
        else look pos (rest, n - 1)
    | _ -> print_string "0.0"

  let error pos msg =
    any_errors := true;
    print_string !file_name;
    look pos (!line_pos, !line_num);
    List.iter print_string [ ":"; msg; "\n" ]

  exception Impossible

  let impossible msg =
    List.iter print_string [ "Error: Compiler bug: "; msg; "\n" ];
    flush stdout;
    raise Impossible
end
