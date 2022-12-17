%token EOF
%token LCURLY RCURLY
%token LPAREN RPAREN
%token LSQUARE RSQUARE
%token ASSIGN COLON COMMA DOT SEMICOLON
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ LT LE GT GE
%token AND OR
%token KW_ARRAY KW_NIL
%token KW_IF KW_THEN KW_ELSE
%token KW_FOR KW_WHILE KW_BREAK
%token KW_LET KW_ASSIGN KW_IN
%token KW_DO KW_END KW_OF KW_TO
%token KW_FUNCTION KW_VAR KW_TYPE
%token <string> ID
%token <int> INT
%token <string> STRING

%start <Ast.dec list> decs
%%

let decs := list(dec)

let dec :=
	| f=fun_dec; <`FunDec>
	| t=ty_dec; <`TyDec>
	| v=var_dec; <`VarDec>

let expseq := separated_nonempty_list(SEMICOLON, exp)

let exp :=
	| array_type=ty_id; LSQUARE; n=exp; RSQUARE; KW_OF; v=exp; <`ArrayExp>
	| bind=lvalue; ASSIGN; e=exp; <`AssignExp>
	| KW_BREAK; {`BreakExp}
	| KW_FOR; c=ID; ASSIGN; _start=exp; KW_TO; _end=exp; KW_DO; body=exp; <`ForExp>
	| fn=ID; LPAREN; args=separated_list(COMMA, exp); RPAREN; <`FunCallExp>
	| KW_IF; cond=exp; KW_THEN; consequence=exp; els=option(KW_ELSE; e=exp; <>); <`IfExp>
	| int=INT; <`IntLitExp>
	| KW_LET; d=decs; KW_IN; e=expseq; KW_END; <`LetExp>
	| v=lvalue; <`LValExp>
	| KW_NIL; {`NilExp}
	| record_type=ty_id; LCURLY;
		fields=separated_list(COMMA, field=ID; EQ; e=exp; <>);
		RCURLY; <`RecordExp>
	| LPAREN; es=expseq; RPAREN; <`SeqExp>
	| string=STRING; <`StringLitExp>
	| KW_WHILE; cond=exp; KW_DO; consequence=exp; <`WhileExp>


let fun_dec :=
	KW_FUNCTION; name=ID;
	LPAREN; params=ty_fields; RPAREN;
	return_type=option(COLON; t=ty_id; <>); 
	EQ; body=exp;
	<>

let lvalue :=
	| array=lvalue; LSQUARE; index=exp; RSQUARE; <`ArrayAccessLVal>
	| record=lvalue; DOT; field=ID; <`FieldLVal>
	| i=ID; <`VarLVal>

let ty_dec := KW_TYPE; name=ty_id; EQ; typ=ty; <`Type>

let ty :=
	| KW_ARRAY; KW_OF; item_type=ty_id; <`TypeArray>
	| type_name=ty_id; <`TypeName>
	| LCURLY; fields=ty_fields; RCURLY; <`TypeRecord>

let ty_fields := separated_list(COMMA, i=ID; COLON; t=ty_id; <>)

let ty_id := ID

let var_dec :=
	KW_VAR; variable=ID;
	type_spec=option(COLON; ts=ty_id; <>);
	ASSIGN; expression=exp;
	<>
