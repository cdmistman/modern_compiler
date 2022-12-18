module Parser : sig
  val parse_file : string -> Ast.exp
  val parse_string : string -> Ast.exp
end = struct
  let parse_file file_name =
    Errormsg.reset ();
    Errormsg.file_name := file_name;
    let file_chan = open_in file_name in
    let tokenizer = Lexing.from_channel file_chan in
    Parse.prog Lex.read tokenizer

  let parse_string input =
    Errormsg.reset ();
    Errormsg.file_name := "<string input>";
    let tokenizer = Lexing.from_string input in
    Parse.prog Lex.read tokenizer
end
