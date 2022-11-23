type id = string
type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp

let rec maxargs (stmt : stm) : int =
  match stmt with
  | CompoundStm (l, r) -> max (maxargs l) (maxargs r)
  | AssignStm _ -> 0
  | PrintStm list -> List.length list

type table = (id * int) list

let update (t : table) (v : id * int) : table = v :: t

let rec lookup (t : table) (name : id) : int =
  match t with
  | (n, v) :: _ when n == name -> v
  | _ :: rest -> lookup rest name
  | [] -> failwith ("variable " ^ name ^ " not defined")

let interp (prog : stm) : unit =
  let rec interpStm (stmt : stm) (env : table) : table =
    match stmt with
    | CompoundStm (l, r) -> interpStm r (interpStm l env)
    | AssignStm (name, expr) ->
        let value, env' = interpExp expr env in
        update env' (name, value)
    | PrintStm exprs ->
        let line, env' =
          List.fold_left
            (fun (line, env) expr ->
              let value, env' = interpExp expr env in
              (line ^ string_of_int value, env'))
            ("", env) exprs
        in
        print_endline line;
        env'
  and interpExp (expr : exp) (env : table) : int * table =
    match expr with
    | IdExp name -> (lookup env name, env)
    | NumExp num -> (num, env)
    | OpExp (l, op, r) ->
        let l', env' = interpExp l env in
        let r', env'' = interpExp r env' in
        ( (match op with
          | Plus -> l' + r'
          | Minus -> l' - r'
          | Times -> l' * r'
          | Div -> l' / r'),
          env'' )
    | EseqExp (stmt, expr) ->
        let res, _ = interpExp expr (interpStm stmt env) in
        (res, env)
  in
  ignore (interpStm prog [])

let prog =
  CompoundStm
    ( AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
      CompoundStm
        ( AssignStm
            ( "b",
              EseqExp
                ( PrintStm [ IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1) ],
                  OpExp (NumExp 10, Times, IdExp "a") ) ),
          PrintStm [ IdExp "b" ] ) )

let%expect_test "basic program" =
  interp prog;
  [%expect {|
    8 7
    80
  |}]
