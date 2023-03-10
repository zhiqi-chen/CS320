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

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = disj

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
  
(* end of parser combinators *)

(* part2 AST *)

type name = string

type const =
  | I of int
  | N of string
  | U

type cmd =
  | Push of const
  | Add
  | Sub
  | Mul
  | Div
  | Trace
  | Ifgz  of cmds * cmds

  | Let
  | Lookup
  | Block of cmds
  | Fun   of string * string * cmds
  | Call

and cmds = cmd list

(* part2 parser *)

let reserved = [
  "Push";
  "Pop";
  "Add";
  "Sub";
  "Mul";
  "Div";
  "Trace";
  "Let";
  "Lookup";
  "Begin";
  "If";
  "Else";
  "Fun";
  "End";
  "Call";
]

let name : string parser =
  let* xs1 = many1 (satisfy (fun c -> is_alpha c || c = '_')) in
  let* xs2 = 
    many (satisfy (fun c ->
      is_alphanum c ||
      (c = '_') ||
      (c = '\'')))
  in
  let s = (implode xs1) ^ (implode xs2) in
  if List.exists (fun x -> x = s) reserved
  then fail
  else pure s << ws

  let nat_parser () =
    let* n = natural in
    pure (I n) << ws
  
  let name_parser () =
    let* n = name in
    pure (N n)
  
  let unit_parser () =
    let* _ = keyword "()" in
    pure (U)
  
  let const_parser () =
    nat_parser () <|>
    name_parser () <|>
    unit_parser ()
  
  let rec push_parser () =
    let* _ = keyword "Push" in
    let* cst = const_parser () in
    pure (Push cst)
  
  and add_parser () =
    let* _ = keyword "Add" in
    pure (Add)
  
  and sub_parser () =
    let* _ = keyword "Sub" in
    pure (Sub)
  
  and mul_parser () =
    let* _ = keyword "Mul" in
    pure (Mul)
  
  and div_parser () =
    let* _ = keyword "Div" in
    pure (Div)
  
  and trace_parser () =
    let* _ = keyword "Trace" in
    pure (Trace)
  
  and let_parser () =
    let* _ = keyword "Let" in
    pure (Let)
  
  and lookup_parser () =
    let* _ = keyword "Lookup" in
    pure (Lookup)
  
  and block_parser () =
    let* _ = keyword "Begin" in
    let* cmds = cmds_parser () in
    let* _ = keyword "End" in
    pure (Block cmds)
  
  and ifgz_parser () =
    let* _ = keyword "If" in
    let* cmds1 = cmds_parser () in
    let* _ = keyword "Else" in
    let* cmds2 = cmds_parser () in
    let* _ = keyword "End" in
    pure (Ifgz (cmds1, cmds2))
  
  and fun_parser () =
    let* _ = keyword "Fun" in
    let* fn = name in
    let* arg = name in
    let* bod = cmds_parser () in
    let* _ = keyword "End" in
    pure (Fun (fn, arg, bod))
  
  and call_parser () =
    let* _ = keyword "Call" in
    pure (Call)
  
  and cmd_parser () = 
    push_parser () <|>
    add_parser () <|>
    sub_parser () <|>
    mul_parser () <|>
    div_parser () <|>
    trace_parser () <|>
    let_parser () <|>
    lookup_parser () <|>
    block_parser () <|>
    ifgz_parser () <|>
    fun_parser () <|>
    call_parser ()
  
  and cmds_parser () =
    many (cmd_parser ())
  
  let parse_cmds s = parse (ws >> cmds_parser ()) s

(* interpreter *)

type value =
  | IVal of int
  | NVal of string
  | UVal
  | FVal of string *
            string *
            cmds *
            env

and env = (string * value) list

type stack = value list

type result =
  | Ok of stack
  | Error

let string_of_value v =
  match v with
  | IVal n -> string_of_int n
  | NVal s -> s
  | UVal -> "()"
  | FVal _ -> "<fun>"

let string_of_result res =
  match res with
  | Ok (v :: _) -> string_of_value v
  | _ -> "Error"

let string_of_log log = 
  let rec loop log =
    match log with
    | [] -> ""
    | s :: [] -> s
    | s :: log -> s ^ "; " ^ loop log
  in
  "[" ^ loop log ^ "]"

let stack_checker st =
  List.for_all 
    (fun v ->
      match v with
      | IVal i -> i >= 0
      | _ -> true) st

let rec interp st env cmds log =
  let _ = 
    if not (stack_checker st) then failwith "negative"
  in
  match cmds with
  | (Push (I n)) :: cmds ->
    interp ((IVal n) :: st) env cmds log
  | (Push (N s)) :: cmds ->
    interp ((NVal s) :: st) env cmds log
  | (Push U) :: cmds ->
    interp (UVal :: st) env cmds log
  | Add :: cmds -> (
    match st with
    | IVal n :: IVal m :: st -> interp (IVal (m + n) :: st) env cmds log
    | _ -> (Error, log))
  | Sub :: cmds -> (
    match st with
    | IVal n :: IVal m :: st -> interp (IVal (m - n) :: st) env cmds log
    | _ -> (Error, log))
  | Mul :: cmds -> (
    match st with
    | IVal n :: IVal m :: st -> interp (IVal (m * n) :: st) env cmds log
    | _ -> (Error, log))
  | Div :: cmds -> (
    match st with
    | IVal 0 :: IVal _ :: _ -> (Error, log)
    | IVal n :: IVal m :: st -> interp (IVal (m / n) :: st) env cmds log
    | _ -> (Error, log))
  | Trace :: cmds -> (
    match st with
    | v :: st -> interp (UVal :: st) env cmds (string_of_value v :: log)
    | _ -> (Error, log))
  | Ifgz (cmds1, cmds2) :: cmds -> (
    match st with
    | IVal n :: st ->
      if n > 0
      then interp st env (cmds1 @ cmds) log
      else interp st env (cmds2 @ cmds) log
    | _ -> (Error, log))
  | Let :: cmds -> (
    match st with
    | v :: NVal s :: st ->
      interp st ((s, v) :: env) cmds log
    | _ -> (Error, log))
  | Lookup :: cmds -> (
    match st with
    | NVal s :: st -> (
      match List.assoc_opt s env with
      | Some v -> interp (v :: st) env cmds log
      | None -> (Error, log))
    | _ -> (Error, log))
  | Block local :: cmds -> (
    match interp [] env local log with
    | (Ok (v :: _), log) -> interp (v :: st) env cmds log
    | _ -> (Error, log))
  | Fun (fn, arg, bod) :: cmds ->
    let fv = FVal (fn, arg, bod, env) in
    interp st ((fn, fv) :: env) cmds log
  | Call :: cmds -> (
    match st with
    | v :: FVal (fn, arg, bod, env') ::st -> (
      let env' = (fn, FVal (fn, arg, bod, env')) :: env' in
      let env' = (arg, v) :: env' in
      match interp [] env' bod log with
      | (Ok (v :: _), log) -> interp (v :: st) env cmds log
      | _ -> (Error, log))
    | _ -> (Error, log))
  | [] -> (Ok st, log)

(* Interprets a program written in the Part2 Stack Language.
 * Required by the autograder, do not change its type. *)
let interpreter src =
  match parse_cmds src with
  | Some (cmds, []) -> (
    match interp [] [] cmds [] with
    | Ok (v :: _), logs -> (string_of_value v, logs)
    | _ -> ("Error", []))
  | _ -> ("Error", [])

(* Testing function. *)
let main fname = 
  let src = readlines fname in
  let (res, log) = interpreter src in
  print_endline (res ^ " " ^ string_of_log log)