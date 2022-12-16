module TOKENS : sig
  type linenum = int
  type token = string

  (*
		In the starter code, these functions are uppercase (and can thus escape
		keyword detection). However, in ocaml, `val` forms must use a lower-case
		identifer, so escaping is done by appending an `_` to the rule name.

		https://v2.ocaml.org/manual/modtypes.html#start-section
	*)
  val type_ : linenum * linenum -> token
  val var : linenum * linenum -> token
  val function_ : linenum * linenum -> token
  val break : linenum * linenum -> token
  val of_ : linenum * linenum -> token
  val end_ : linenum * linenum -> token
  val in_ : linenum * linenum -> token
  val nil : linenum * linenum -> token
  val let_ : linenum * linenum -> token
  val do_ : linenum * linenum -> token
  val to_ : linenum * linenum -> token
  val for_ : linenum * linenum -> token
  val while_ : linenum * linenum -> token
  val else_ : linenum * linenum -> token
  val then_ : linenum * linenum -> token
  val if_ : linenum * linenum -> token
  val array : linenum * linenum -> token
  val assign : linenum * linenum -> token
  val or_ : linenum * linenum -> token
  val and_ : linenum * linenum -> token
  val ge : linenum * linenum -> token
  val gt : linenum * linenum -> token
  val le : linenum * linenum -> token
  val lt : linenum * linenum -> token
  val neq : linenum * linenum -> token
  val eq : linenum * linenum -> token
  val divide : linenum * linenum -> token
  val times : linenum * linenum -> token
  val minus : linenum * linenum -> token
  val plus : linenum * linenum -> token
  val dot : linenum * linenum -> token
  val rbrace : linenum * linenum -> token
  val lbrace : linenum * linenum -> token
  val rbrack : linenum * linenum -> token
  val lbrack : linenum * linenum -> token
  val rparen : linenum * linenum -> token
  val lparen : linenum * linenum -> token
  val semicolon : linenum * linenum -> token
  val colon : linenum * linenum -> token
  val comma : linenum * linenum -> token
  val eof : token

  (* the textbook includes the first parameter as part of the tuple, but that
     isn't as convenient with my `finish` helper *)
  val string : string -> linenum * linenum -> token
  val int : int -> linenum * linenum -> token
  val id : string -> linenum * linenum -> token
end
