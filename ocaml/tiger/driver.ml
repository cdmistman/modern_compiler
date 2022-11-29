module Parse = struct
  let parse filename =
    let rec print_tokens lexer =
      let token = Lex.read lexer in
      print_endline token;
      if String.sub token 0 3 = "EOF" then () else print_tokens lexer
    in
    open_in filename |> Lexing.from_channel |> print_tokens
end