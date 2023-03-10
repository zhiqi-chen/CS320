(* util functions *)

let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* types *)

type nat = int

and name = string

and env = (name * value) list

and closure = name * name * coms * env

and const = 
  | I of int
  | S of string
  | N of name
  | U
  | E

and com = 
  | Push of const | Trace
  | Add | Sub | Mul | Div
  | IfElseEnd of coms * coms
  | Let
  | Lookup
  | BeginEnd of coms
  | FunEnd of name * name * coms
  | Call

and coms = com list

and prog = coms

and value = 
  | IVal of int
  | SVal of string
  | CVal of closure
  | NVal of name
  | UVal
  | EVal

and stack = value list

(* end of types *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let letter : char parser =
  satisfy is_alpha

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

let nat_parser : int parser = 
  natural

let name_parser : string parser = 
  let* c_initial = (letter <|> char '_') in
  let* cs = many (letter <|> digit <|> char '_' <|> char '\'') in
  pure (implode (c_initial::cs))

let unit_parser : unit parser = 
  let* c1 = char '(' in
  let* c2 = char ')' in
  pure () 

let const_parser : const parser =
  (nat_parser >|= fun i -> I i) <|> 
  (name_parser >|= fun n -> N n) <|> 
  (unit_parser >|= fun u -> U)

let push_parser : com parser =
  let* _ = keyword "Push" in
  let* const = const_parser << ws in
  pure (Push const)

let trace_parser : com parser =
  let* _ = keyword "Trace" in
  pure Trace

let add_parser : com parser =
  let* _ = keyword "Add" in
  pure Add

let sub_parser : com parser =
  let* _ = keyword "Sub" in
  pure Sub

let mul_parser : com parser =
  let* _ = keyword "Mul" in
  pure Mul

let div_parser : com parser =
  let* _ = keyword "Div" in
  pure Div

let let_parser : com parser = 
  let* _ = keyword "Let" in 
  pure Let

let lookup_parser : com parser =
  let* _ = keyword "Lookup" in
  pure Lookup

let call_parser : com parser =
  let* _ = keyword "Call" in
  pure Call

let rec ifelseend_parser() : com parser =
  let* _ = keyword "If" in
  let* if_coms = coms_parser() in
  let* _ = keyword "Else" in
  let* else_coms = coms_parser() in
  let* _ = keyword "End" in
  pure (IfElseEnd (if_coms, else_coms))

and beginend_parser() : com parser = 
  let* _ = keyword "Begin" in 
  let* coms = coms_parser() in
  let* _ = keyword "End" in
  pure (BeginEnd (coms))

and funend_parser() : com parser =
  let* _ = keyword "Fun" in 
  let* fname = name_parser in 
  let* arg = name_parser in 
  let* coms = coms_parser() in
  pure (FunEnd (fname, arg, coms))

and com_parser() : com parser =
  push_parser <|> 
  trace_parser <|> 
  add_parser <|> 
  sub_parser <|> 
  mul_parser <|> 
  div_parser <|> 
  let_parser <|>
  lookup_parser <|>
  ifelseend_parser() <|>
  beginend_parser() <|>
  funend_parser() <|>
  call_parser

and coms_parser() = many1 (com_parser())

let prog_parser : prog parser =
  let* com = com_parser() in
  let* prog =
    many (let* _ = keyword "\n" <|> ws in com_parser())
  in 
  pure (com::prog)

(* end of parser combinators *)

(* evaluation *)

let some_const c env =
  match c with
  | N n -> List.assoc_opt n env
  | c -> Some c

let value_to_string v : string =
  match v with
  | IVal i -> string_of_int i
  | SVal s -> s
  | NVal n -> n
  | UVal -> "()"
  | CVal c -> "<fun>"
  | EVal -> "Error"

let const_to_name n =
  match n with
  | N n -> Some n
  | _ -> None


let rec lookup (n : name) (env : env) : value option =
  match env with 
  | (bn, bv) :: envs -> 
    if bn = n then Some bv
    else lookup n envs
  | [] -> None

let rec eval (p : prog) (s : stack) env =
  match p, s, env with
  | Push (I i) :: p, _, env -> eval p ((IVal(i)) :: s) env
  | Push (S ss) :: p, _, env -> eval p (SVal(ss) :: s) env
  | Push (N n) :: p, _, env -> eval p (NVal(n) :: s) env
  | Push U :: p, _, env -> eval p (UVal :: s) env
  | Push E :: p, _, env -> None
  | Trace :: p, _ :: s, env -> eval p (UVal :: s) env
  | Trace :: p, _, env -> None
  | Add :: p, IVal v2 :: IVal v1 :: s, env -> eval p (IVal (v1 + v2) :: s) env
  | Add :: p, _, env -> None
  | Sub :: p, IVal v2 :: IVal v1 :: s, env -> eval p (IVal (v1 - v2) :: s) env
  | Sub :: p, _, env -> None
  | Mul :: p, IVal v2 :: IVal v1 :: s, env -> eval p (IVal (v1 * v2) :: s) env
  | Mul :: p, _, env -> None
  | Div :: p, IVal v2 :: IVal v1 :: s, env -> eval p (IVal (v1 / v2) :: s) env
  | Div :: p, _, env -> None
  | IfElseEnd (if_coms, else_coms) :: p, IVal v :: vs, env ->
    if v > 0 then (
      match if_coms with
      | [] -> None
      | if_coms -> eval (if_coms @ p) vs env
    )
    else (
      match else_coms with 
      | [] -> None
      | else_coms -> eval (else_coms @ p) vs env
    )
  | IfElseEnd (if_coms, else_coms) :: p, _, env -> None
  | Let :: p, v2 :: NVal v1 :: s, env -> eval p s ((v1, v2) :: env) (* v1 = name; v2 = value *)
  | Let :: p, _, env -> None
  | Lookup :: p, NVal v :: s, env -> (
      match lookup v env with 
      | Some bv -> eval p (bv :: s) env 
      | None -> None (* if name is not bound in the current environemtn, terminate and report error *)
    )
  | Lookup :: p, _, env -> None
  | BeginEnd (inner_coms) :: p, s, env -> (
      match eval inner_coms [] env with
      | Some (v :: _) -> eval p (v :: s) env
      | Some [] -> eval p s env
      | None -> None
    )
  (* closure value being bound to fname *)
  | FunEnd (fname, arg, inner_coms) :: p, s, env -> let closure = CVal (fname, arg, inner_coms, env) in
    eval p s ((fname, closure) :: env)
  | Call :: p, v :: CVal (fname, arg, inner_coms, save_env) :: s, env -> (
      (* argument value being bound to arg *)
      let new_env = (arg, v) :: (fname, CVal (fname, arg, inner_coms, save_env)) :: save_env in
      match eval inner_coms [] new_env with
      | Some (v :: _) -> eval p (v :: s) env
      | Some [] -> eval p s env
      | None -> None
    )
  | Call :: p, _, env -> None
  | [], _, _ -> Some s

let rec save (p : prog) (s : stack) (log : stack) env =
  match p, s, log, env with
  | Push (I i) :: p, _, log, env -> save p (IVal i :: s) log env
  | Push (S ss) :: p, _, log, env -> save p (SVal ss :: s) log env
  | Push (N n) :: p, _, log, env -> save p (NVal n :: s) log env
  | Push U :: p, _, log, env -> save p (UVal :: s) log env
  | Push E :: p, _, log, env -> None
  | Trace :: p, v :: s, log, env -> save p (UVal :: s) (v :: log) env
  | Trace :: p, _, log, env -> None
  | Add :: p, IVal v2 :: IVal v1 :: s, log, env -> save p (IVal (v1 + v2) :: s) log env
  | Add :: p, _, log, env -> None
  | Sub :: p, IVal v2 :: IVal v1 :: s, log, env -> save p (IVal (v1 - v2) :: s) log env
  | Sub :: p, _, log, env -> None
  | Mul :: p, IVal v2 :: IVal v1 :: s, log, env -> save p (IVal (v1 * v2) :: s) log env
  | Mul :: p, _, log, env -> None
  | Div :: p, IVal v2 :: IVal v1 :: s, log, env -> save p (IVal (v1 / v2) :: s) log env
  | Div :: p, _, log, env -> None
  | IfElseEnd (if_coms, else_coms) :: p, IVal v :: vs, log, env ->
    if v > 0 then (
      match if_coms with
      | [] -> None
      | if_coms -> save (if_coms @ p) vs log env
    )
    else (
      match else_coms with 
      | [] -> None
      | else_coms -> save (else_coms @ p) vs log env
    )
  | IfElseEnd (if_coms, else_coms) :: p, _, log, env -> None
  | Let :: p, v2 :: NVal v1 :: s, log, env -> save p s log ((v1, v2) :: env) (* v1 = name; v2 = value *)
  | Let :: p, _, log, env -> None
  | Lookup :: p, NVal v :: s, log, env -> (
      (* if the value is a name *)
      match lookup v env with 
      | Some bv -> save p (bv :: s) log env 
      | None -> None (* if name is not bound in the current environemtn, terminate and report error *)
    )
  | Lookup :: p, _, log, env -> None
  | BeginEnd (inner_coms) :: p, s, log, env -> (
      match save inner_coms [] log env, eval inner_coms [] env with
      | Some l , Some (v :: _) -> save p (v :: s) l env
      | _ -> None
    )
  (* closure value being bound to fname *)
  | FunEnd (fname, arg, inner_coms) :: p, s, log, env -> let closure = CVal (fname, arg, inner_coms, env) in
    save p s log ((fname, closure) :: env)
  | Call :: p, v :: CVal (fname, arg, inner_coms, save_env) :: s, log, env -> (
      (* argument value being bound to arg *)
      let new_env = (arg, v) :: (fname, CVal (fname, arg, inner_coms, save_env)) :: save_env in
      match save inner_coms [] log new_env, eval inner_coms [] new_env with
      | Some l, Some (sv :: _) -> save p (sv :: s) l env
      | _ -> None
    )
  | Call :: p, _, log, env -> None
  | [], _, log, _ -> Some log

(* end of evaluation *)

(* Interprets a program written in the Part1 Stack Language.
 * Required by the autograder, do not change its type. *)

let left_string (s : string) : stack option =
  match parse prog_parser s with
  | Some (p, []) -> eval p [] []
  | _ -> None

let right_stack (s : string) : stack option =
  match parse prog_parser s with
  | Some (p, []) -> save p [] [] []
  | _ -> None

let right_string (s: string) : string list =
  let right = right_stack s in
  let rec rux right acc = 
    match right with 
    | None -> []
    | Some [] -> acc
    | Some (h::t) -> rux (Some t) (List.rev (value_to_string(h) :: List.rev(acc)))
  in rux right []

let interpreter (src : string) : (string * string list) =
  let left = left_string src in
  let ss = right_string src in
  match left with
  | Some (s::_) -> (value_to_string s, ss)
  | Some [] -> ("()", ss)
  | None -> ("Error", [])


let test1 = 
  "Begin
Push x
Push 20
Let
Begin
Push y
Push 23
Let
Push x
Lookup
Push y
Lookup
Add
End
End"

let test2 = 
  "Begin
Push x
Begin
Push x
Push 133
Let
Begin
Push y
Push 21
Let
Push y
Lookup
End
End
Let
Push x
Lookup
End"

let test3 = 
  "Begin
Push x
Begin
Push x
Push 0
Let
Begin
Push y
Push 1
Let
Push x
Lookup
End
End
Let
Begin
Push x
Push x
Lookup
If
Begin
Push 123
End
Else
Begin
Push 312
End
End
Let
Push x
Lookup
End
End"

let test4 =
  "Begin
Push x
Begin
Push x
Push 34
Push 0
Sub
Let
Begin
Push y
Push 45
Push 0
Sub
Let
Push x
Lookup
End
End
Let
Begin
Push x
Push x
Lookup
If
Begin
Push 23
End
Else
Begin
Push 31
End
End
Let
Push x
Lookup
End
End"

let test5 = 
  "Begin
Push x
Begin
Push x
Push 12
Push 0
Sub
Let
Begin
Push _
Push x
Lookup
Trace
Let
Begin
Push y
Push 32
Push 0
Sub
Let
Begin
Push _
Push y
Lookup
Trace
Let
Push x
Lookup
End
End
End
End
Let
Begin
Push x
Push x
Lookup
If
Begin
Push 99
Trace
End
Else
Begin
Push 13
Trace
End
End
Let
Push x
Lookup
End
End"

let test6 = 
  "Begin
Push x
Push 23
Let
Begin
Push y
Push 5
Let
Begin
Push x
Push x
Lookup
Push y
Lookup
Div
Let
Begin
Push _
Push x
Lookup
Trace
Let
Begin
Push x
Push x
Lookup
Push y
Lookup
Div
Let
Begin
Push _
Push x
Lookup
Trace
Let
Push x
Lookup
If
Begin
Push 4
End
Else
Begin
Push 0
Push 0
Sub
End
End
End
End
End
End
End
End"

let test7 =
  "Begin
Push loop
Fun loop n
Push n
Lookup
If
Begin
Begin
Push _
Push n
Lookup
Trace
Let
Push loop
Lookup
Push n
Lookup
Push 2
Sub
Call
End
End
Else
Begin
Push ()
End
End
End
Push loop
Lookup
Let
Push loop
Lookup
Push 34
Call
End"

let test8 =
  "Begin
Push fact
Fun fact n
Push n
Lookup
If
Begin
Push n
Lookup
Push fact
Lookup
Push n
Lookup
Push 1
Sub
Call
Mul
End
Else
Begin
Push 1
End
End
End
Push fact
Lookup
Let
Push fact
Lookup
Push 8
Call
End"

let test9 = 
  "Begin
Push eq
Fun eq x
Fun _ y
Push x
Lookup
If
Begin
Push y
Lookup
If
Begin
Push eq
Lookup
Push x
Lookup
Push 1
Sub
Call
Push y
Lookup
Push 1
Sub
Call
End
Else
Begin
Push 0
End
End
End
Else
Begin
Push y
Lookup
If
Begin
Push 0
End
Else
Begin
Push 1
End
End
End
End
End
Push _
Lookup
End
Push eq
Lookup
Let
Begin
Push _
Push eq
Lookup
Push 10
Call
Push 10
Call
Trace
Let
Begin
Push _
Push eq
Lookup
Push 1
Call
Push 2
Push 1
Sub
Call
Trace
Let
Begin
Push _
Push eq
Lookup
Push 9
Call
Push 3
Push 3
Mul
Call
Trace
Let
Begin
Push _
Push eq
Lookup
Push 25
Push 5
Div
Call
Push 25
Push 6
Div
Call
Trace
Let
Push ()
End
End
End
End
End"

let test =
  "Fun f x
Push x
Push 2
End
Push x
Lookup
Push 3
Call
Push 1"

let _ = interpreter test7

(* #use "part1_template.ml";; *)