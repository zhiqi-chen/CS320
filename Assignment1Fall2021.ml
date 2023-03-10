(*
Honor code comes here:
​
First Name: Zhiqi
Last Name: Chen
BU ID: U97832308
​
I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(*
a print_list function you find useful for debugging.
*)
let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l
  in let _ = print_string "[" 
  in let _ = aux ls
  in print_string "]" 

(*
TODO: Write a tail recursive function equalLists testing the equality 
of two lists of integers.
For example,
equalLists [1;2;3;4] [1;2;3;4] = true
equalLists [1;2;3;4] [2;3;4;5] = false
equalLists [1;2;3;4] [1] = false
*)



let rec equalLists (x: int list) (y: int list): bool = 
  match (x, y) with
    ([], []) -> true
  |
    ([], _) -> false
  |
    (_, []) -> false
  |
    ((hx::tx), (hy::ty)) ->
    if hx != hy then false
    else equalLists tx ty;;


(*

TODO:
The function list_of_list_of_int_as_string takes in as input a list of list of 
int and evaluates into its string representation. 

Here are some test cases on how list_of_list_of_int_as_string must work: 
list_of_list_of_int_as_string [[1];[2]] will evaluate into "[[1];[2]]"
list_of_list_of_int_as_string [[1;2;3]] will evaluate into "[[1;2;3]]"
list_of_list_of_int_as_string [[1];[2];[3];[4]] will evaluate into "[[1];[2];[3];[4]]"
list_of_list_of_int_as_string [[1]] will evaluate into "[[1]]"
list_of_list_of_int_as_string [] will evaluate into "[]"
*)



let list_of_int_as_string (l: int list): string =
  let rec aux = fun (x: int list) ->
    match x with
      [] -> ""
    |
      [h] -> string_of_int h
    |
      h::t -> string_of_int h ^ "; " ^ aux t
  in aux l

let list_of_list_of_int_as_string (l: int list list): string =
  let rec aux = fun (y: int list list) ->
    match y with 
      [] -> ""
    |
      [h] -> "[" ^ list_of_int_as_string h ^ "]"
    |
      h::t -> "[" ^ list_of_int_as_string h ^ "]" ^ "; " ^ aux t
  in "[" ^ aux l ^ "]" 




(*
TODO:
Write a function called `repeat` that given a number and a length:
returns a list containing that number, n times.
If the length is zero or negative it should return an empty list.
The solution should be tail recursive
For example,
  repeat 4 7 = [4; 4; 4; 4; 4; 4; 4]
  repeat 3 3 = [3; 3; 3]
  repeat 10 0 = []
  repeat 4 1000000 does not stack overflow
*)



let rec repeat = fun x n ->
  let rec aux = fun accum x1 n1 ->
    match (x1, n1) with     
      (x1, n1) ->
      if n1 = 0 then accum
      else if n1 <= 0 then []
      else aux (x1::accum) x1 (n1-1)
  in aux [] x n




(* 
TODO:
Hint: use the following helper function 
let rec insert (i: int) (list: int list): int list = fail with "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number unsorted
for example,

insert 5 [1;3;5;7] = [1;3;5;5;7]
*)


let rec insert (i: int) (ls: int list) =
  match (i, ls) with
    i, ls  -> List.sort compare ([i] @ ls)

let rec sort (ls: int list): int list =
  match ls with
  | [] -> []
  | n :: tail -> insert n (sort tail)