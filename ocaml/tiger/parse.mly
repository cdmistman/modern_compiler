%token EOF
(* braces *)
%token LCURLY "{" RCURLY "}"
%token LPAREN "(" RPAREN ")"
%token LSQUARE "[" RSQUARE "]"
(* symbols *)
%token ASSIGN ":="
%token COLON ":"
%token COMMA ","
%token DOT "."
%token SEMICOLON ";"
(* arithmetic operators *)
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIVIDE "/"
(* comparison operators *)
%token EQ "="
%token NEQ "<>"
%token LT "<"
%token LE "<="
%token GT ">"
%token GE ">="
(* logical operators *)
%token AND "&"
%token OR "|"
(* keywords *)
%token KW_ARRAY "array"
%token KW_BREAK "break"
%token KW_DO "do"
%token KW_ELSE "else"
%token KW_END "end"
%token KW_FOR "for"
%token KW_FUNCTION "function"
%token KW_IF "if"
%token KW_IN "in"
%token KW_LET "let"
%token KW_NIL "nil"
%token KW_OF "of"
%token KW_THEN "then"
%token KW_TYPE "type"
%token KW_TO "to"
%token KW_VAR "var"
%token KW_WHILE "while"
(* user-defined tokens *)
%token <string> ID
%token <int> INT
%token <string> STRING

(*
precedences listed from low to high

I'm using some of the precedences from ocaml's `parser.mly`
https://github.com/ocaml/ocaml/blob/be210179503c4a82b72dd4762560e13c408d37b7/parsing/parser.mly
*)
%nonassoc "in"
%nonassoc ";"
%nonassoc "let"
%nonassoc "function" "type" "var"
%nonassoc "then" "do" "of" /* below KW_ELSE */
%left "else"
%right ":="
%left "|"
%left "&"
%nonassoc "=" "<>" "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/"
// unary negation operator is highest-precedence but you can't assign a token
// multiple precedences - instead, this is handled in the `exp` rule by setting
// the highest precedence using the `highest_prec` precedence
// %right MINUS
%nonassoc highest_prec

%start <Ast.exp> prog
%%

let prog := e=exp; EOF; <>

let arith_binop :=
	| "/"; {`DividedBy}
	| "-"; {`Minus}
	| "+"; {`Plus}
	| "*"; {`Times}

let bool_binop :=
	| "&"; {`And}
	| "|"; {`Or}

let cmp_binop :=
	| "="; {`Eq}
	| ">="; {`Ge}
	| ">"; {`Gt}
	| "<="; {`Le}
	| "<"; {`Lt}
	| "<>"; {`Neq}

let exp :=
	| lhs=exp; o=arith_binop; rhs=exp; <`ArithBinExp>
	| array_type=ty_id; "["; n=exp; "]"; "of"; v=exp; <`ArrayExp>
	| bind=lvalue; ":="; e=exp; <`AssignExp>
	| lhs=exp; o=bool_binop; rhs=exp; <`BoolBinExp>
	| "break"; {`BreakExp}
	| lhs=exp; o=cmp_binop; rhs=exp; <`CmpBinExp>
	| "for"; c=ID; ":="; _start=exp; "to"; _end=exp; "do"; body=exp; <`ForExp>
	| fn=ID; "("; args=separated_list(",", exp); ")"; <`FunCallExp>
	| "if"; cond=exp; "then"; consequence=exp; els=option("else"; e=exp; <>); <`IfExp>
	| int=INT; <`IntLitExp>
	| "let"; d=decs; "in"; e=expseq; "end"; <`LetExp>
	| v=lvalue; <`LValExp>
	| "nil"; {`NilExp}
	| record_type=ty_id; "{";
		fields=separated_list(",", field=ID; "="; e=exp; <>);
		"}"; <`RecordExp>
	| "("; es=expseq; ")"; <`SeqExp>
	| string=STRING; <`StringLitExp>
	| "while"; cond=exp; "do"; consequence=exp; <`WhileExp>

let decs := list(dec)

let dec :=
	| f=fun_dec; <`FunDec>
	| t=ty_dec; <`TyDec>
	| v=var_dec; <`VarDec>

let expseq := separated_nonempty_list(SEMICOLON, exp)

let fun_dec :=
	"function"; name=ID;
	"("; params=ty_fields; ")";
	return_type=option(":"; t=ty_id; <>); 
	"="; body=exp;
	<>

let lvalue :=
	| array=lvalue; "["; index=exp; "]"; <`ArrayAccessLVal>
	| record=lvalue; "."; field=ID; <`FieldLVal>
	| i=ID; <`VarLVal>

let ty_dec := "type"; name=ty_id; "="; typ=ty; <`Type>

let ty :=
	| "array"; "of"; item_type=ty_id; <`TypeArray>
	| type_name=ty_id; <`TypeName>
	| "{"; fields=ty_fields; "}"; <`TypeRecord>

let ty_fields := separated_list(",", i=ID; ":"; t=ty_id; <>)

let ty_id := ID

let var_dec :=
	"var"; variable=ID;
	type_spec=option(":"; ts=ty_id; <>);
	":="; expression=exp;
	<>
