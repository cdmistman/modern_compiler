{
open Errormsg
open Lexing
open Tokens

let finish lexbuf tok =
	tok (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf);;
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
	| "{" { finish lexbuf TOKENS.lbrace }
	| "}" { finish lexbuf TOKENS.rbrace }
	| "[" { finish lexbuf TOKENS.lbrack }
	| "]" { finish lexbuf TOKENS.rbrack }
	| "(" { finish lexbuf TOKENS.lparen }
	| ")" { finish lexbuf TOKENS.rparen }
	(* punctuation *)
	| "&" { finish lexbuf TOKENS.and_ }
	| ":" { finish lexbuf TOKENS.colon }
	| "," { finish lexbuf TOKENS.comma }
	| "/" { finish lexbuf TOKENS.divide }
	| "." { finish lexbuf TOKENS.dot }
	| "=" { finish lexbuf TOKENS.eq }
	| ">" { finish lexbuf TOKENS.gt }
	| "<" { finish lexbuf TOKENS.lt }
	| "-" { finish lexbuf TOKENS.minus }
	| "|" { finish lexbuf TOKENS.or_ }
	| "+" { finish lexbuf TOKENS.plus }
	| ";" { finish lexbuf TOKENS.semicolon }
	| "*" { finish lexbuf TOKENS.times }
	| ":=" { finish lexbuf TOKENS.assign }
	| ">=" { finish lexbuf TOKENS.ge }
	| "<=" { finish lexbuf TOKENS.le }
	| "<>" { finish lexbuf TOKENS.neq }
	(* keywords *)
	| "array" { finish lexbuf TOKENS.array }
	| "break" { finish lexbuf TOKENS.break }
	| "do" { finish lexbuf TOKENS.do_ }
	| "else" { finish lexbuf TOKENS.else_ }
	| "end" { finish lexbuf TOKENS.end_ }
	| "for" { finish lexbuf TOKENS.for_ }
	| "function" { finish lexbuf TOKENS.function_ }
	| "if" { finish lexbuf TOKENS.if_ }
	| "in" { finish lexbuf TOKENS.in_ }
	| "let" { finish lexbuf TOKENS.let_ }
	| "nil" { finish lexbuf TOKENS.nil }
	| "of" { finish lexbuf TOKENS.of_ }
	| "then" { finish lexbuf TOKENS.then_ }
	| "to" { finish lexbuf TOKENS.to_ }
	| "type" { finish lexbuf TOKENS.type_ }
	| "var" { finish lexbuf TOKENS.var }
	| "while" { finish lexbuf TOKENS.while_ }
	(* user-defined input *)
	| id { Lexing.lexeme lexbuf |> TOKENS.id |> finish lexbuf }
	| digit+ { Lexing.lexeme lexbuf |> int_of_string |> TOKENS.int |> finish lexbuf }
	| '"' { read_string (Lexing.lexeme_start lexbuf) (Buffer.create 8) lexbuf }
	| eof { TOKENS.eof }
	| _ as ch { "uh oh: " ^ (string_of_int (Char.code ch)) |> ERRORMSG.impossible }

and read_comment level =
	parse
	| "/*" { read_comment (level + 1) lexbuf }
	| "*/" { if level = 0 then read lexbuf else read_comment (level - 1) lexbuf }
	| _ { read_comment level lexbuf }
	| eof {
		ERRORMSG.error lexbuf.lex_curr_pos "unclosed comment";
		TOKENS.eof
	}

and read_string start_p buf =
	parse
	| '"' {
		let end_p = lexbuf.lex_curr_pos in
		TOKENS.string (Buffer.contents buf) (start_p, end_p)
	}
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
			ERRORMSG.error (lexbuf.lex_curr_pos - 3) errormsg
		else begin
			(try Char.chr chr' with
				Invalid_argument _ -> ERRORMSG.impossible errormsg)
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
		ERRORMSG.error end_p "unclosed string";
		TOKENS.string (Buffer.contents buf) (start_p, end_p)
	}
	| _ as c { "unhandled string char `" ^ String.make 1 c ^ "`" |> ERRORMSG.impossible }

