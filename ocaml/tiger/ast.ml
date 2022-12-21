type id = string
and dec = [ `FunDec of fun_dec | `TyDec of ty_dec | `VarDec of var_dec ]

and exp =
  [ `ArithBinExp of exp * exp_arith_binop * exp
  | `ArrayExp of ty_id * exp * exp
  | `AssignExp of lvalue * exp
  | `BoolBinExp of exp * exp_bool_binop * exp
  | `BreakExp
  | `CmpBinExp of exp * exp_cmp_binop * exp
  | `ForExp of id * exp * exp * exp
  | `FunCallExp of id * exp list
  | `IfExp of exp * exp * exp option
  | `IntLitExp of int
  | `LetExp of dec list * exp list
  | `LValExp of lvalue
  | `NegExp of exp
  | `NilExp
  | `RecordExp of ty_id * (id * exp) list
  | `SeqExp of exp list
  | `StringLitExp of string
  | `UnitExp
  | `WhileExp of exp * exp ]

and exp_arith_binop = [ `DividedBy | `Minus | `Plus | `Times ]
and exp_bool_binop = [ `And | `Or ]
and exp_cmp_binop = [ `Eq | `Ge | `Gt | `Le | `Lt | `Neq ]
and fun_dec = id * ty_field list * ty_id option * exp

and lvalue =
  [ `ArrayAccessLVal of lvalue * exp
  | `FieldLVal of lvalue * id
  | `VarLVal of id ]

and ty_dec = [ `Type of ty_id * ty ]

and ty =
  [ `TypeArray of ty_id | `TypeName of ty_id | `TypeRecord of ty_field list ]

and ty_field = id * ty_id
and ty_id = id
and var_dec = id * ty_id option * exp
