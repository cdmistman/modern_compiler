{
open Errormsg
open Lexing
open Parse
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

let id = letter ('_' | digit | letter)*

rule read =
	parse
	(* trivia *)
	| "/*" { read_comment 0 lexbuf }
	| whitespace { read lexbuf }
	| newline { Lexing.new_line lexbuf; read lexbuf }
	(* braces/brackets/parentheses *)
	| "{" { LCURLY }
	| "}" { RCURLY }
	| "(" { LPAREN }
	| ")" { RPAREN }
	| "[" { LSQUARE }
	| "]" { RSQUARE }
	(* punctuation *)
	| "&" { AND }
	| ":" { COLON }
	| "," { COMMA }
	| "/" { DIVIDE }
	| "." { DOT }
	| "=" { EQ }
	| ">" { GT }
	| "<" { LT }
	| "-" { MINUS }
	| "|" { OR }
	| "+" { PLUS }
	| ";" { SEMICOLON }
	| "*" { TIMES }
	| ":=" { ASSIGN }
	| ">=" { GE }
	| "<=" { LE }
	| "<>" { NEQ }
	(* keywords *)
	| "array" { KW_ARRAY }
	| "break" { KW_BREAK }
	| "do" { KW_DO }
	| "else" { KW_ELSE }
	| "end" { KW_END }
	| "for" { KW_FOR }
	| "function" { KW_FUNCTION }
	| "if" { KW_IF }
	| "in" { KW_IN }
	| "let" { KW_LET }
	| "nil" { KW_NIL }
	| "of" { KW_OF }
	| "then" { KW_THEN }
	| "to" { KW_TO }
	| "type" { KW_TYPE }
	| "var" { KW_VAR }
	| "while" { KW_WHILE }
	(* user-defined input *)
	| id { ID (Lexing.lexeme lexbuf) }
	| digit+ { INT (int_of_string (Lexing.lexeme lexbuf)) }
	| '"' { read_string (Lexing.lexeme_start lexbuf) (Buffer.create 8) lexbuf }
	| eof { EOF }
	| _ as ch { "uh oh: " ^ (string_of_int (Char.code ch)) |> ErrorMsg.impossible }

and read_comment level =
	parse
	| "/*" { read_comment (level + 1) lexbuf }
	| "*/" { if level = 0 then read lexbuf else read_comment (level - 1) lexbuf }
	| _ { read_comment level lexbuf }
	| eof {
		ErrorMsg.error lexbuf.lex_curr_pos "unclosed comment";
		EOF
	}

and read_string start_p buf =
	parse
	| '"' { STRING (Buffer.contents buf) }
	| "\\n" | "\\^J" {
		Buffer.add_char buf '\n';
		read_string start_p buf lexbuf
	}
	| "\\t" | "\\^I" {
		Buffer.add_char buf '\t';
		read_string start_p buf lexbuf
	}
	| "\\" (digit digit digit) as chr {
		let errormsg = "invalid ASCII character code " ^ chr in
		let chr' = int_of_string chr in
		if chr' > 255 then
			ErrorMsg.error (lexbuf.lex_curr_pos - 3) errormsg
		else begin
			(try Char.chr chr' with
				Invalid_argument _ -> ErrorMsg.impossible errormsg)
			|> Buffer.add_char buf
		end;
		read_string start_p buf lexbuf
	}
	| "\\\"" { Buffer.add_char buf '"'; read_string start_p buf lexbuf }
	| "\\\\" { Buffer.add_char buf '\\'; read_string start_p buf lexbuf }
	| "\\\\" (whitespace | newline)+ "\\\\" { read_string start_p buf lexbuf }
	| [^ '"' '\\']+ {
		Lexing.lexeme lexbuf |> Buffer.add_string buf;
		read_string start_p buf lexbuf
	}
	| eof {
		let end_p = lexbuf.lex_curr_pos in
		ErrorMsg.error end_p "unclosed string";
		STRING (Buffer.contents buf)
	}
	| _ as c { "unhandled string char `" ^ String.make 1 c ^ "`" |> ErrorMsg.impossible }

