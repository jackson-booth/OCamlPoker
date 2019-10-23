open String

(*Types and exceptions documented in the .mli*)
exception Empty
exception Malformed
exception Retrieve of string
exception ExpectInt
exception PrematureExit

type response = Raise of int | Match | Check |
                Fold | Help | Exit | Save of string

(**[extract_commas s acc] is [s'], where [s'] but with any commas as
   individual elements in the list [s'].
   Example: [extract_commas s] where [s] is ["Picard";"human";"1701,"] is
   ["Picard";"human";"1701";","]
   Raises: [Malformed] if the a case where commas were in weird positions were
   encountered. This should never be encountered in valid input.**)
let rec extract_commas (str:string list) (acc:string list) : string list =
  match str with
  | [] -> List.rev acc
  | h::t -> try
      (let c_index = String.index h ',' in
       let len = (String.length h) in
       if c_index = len - 1 then
         let h_1 = String.sub h 0 (len-1) in
         extract_commas t (","::h_1::acc)
       else raise Malformed)
    with
    | _ -> extract_commas t (h::acc)

(**[remove_empty_spaces s n] is [s] with " " and  "" elements removed,
   but otherwise unchanged.
   Requires: [n] is an empty list when this function is first *)
let rec remove_empty_spaces (str_list: string list)
    (new_str_list:string list) : string list =
  match str_list with
  | [] -> List.rev new_str_list
  | " "::t -> remove_empty_spaces t new_str_list
  | ""::t -> remove_empty_spaces t new_str_list
  | h::t -> remove_empty_spaces t (h::new_str_list)

(**[convert_string_int s] is a integer representation of [s]
Raises: [ExpectInt] if [s] has no integer conversion (eg. [s] is "apple") *)
let convert_string_int (str:string) : int =
  try int_of_string str
  with | Failure s -> raise ExpectInt

(**[convert_string_int s] is a integer representation of [s].
Requires: The function is called during instantiation of a game for proper
  function, if this function is called
Raises: [Malformed] if [s] has no integer conversion (eg. [s] is "apple") *)
let convert_string_int_instantiation (str:string) : int =
  try int_of_string str
  with | Failure s -> raise Malformed

(*Documentation in the .mli*)
let parse (str:string) : response =
  let str_as_list = String.split_on_char  ' ' str in
  let new_str_as_list = remove_empty_spaces str_as_list [] in
  match new_str_as_list with
  | ["fold"] -> Fold
  | ["bet";i] ->  let new_int = convert_string_int i in Raise new_int
  | ["raise";i] ->  let new_int = convert_string_int i in Raise new_int
  | ["match"] -> Match
  | ["check"] -> Check
  | ["help"] -> Help
  | ["exit"] -> Exit
  | ["save"; i] -> Save i
  | [] -> raise Empty
  | _ -> raise Malformed

(*Documentation in the .mli*)
let parse_instantiation (str:string) :(Model.player_info list) * bool =
  let str_as_list = String.split_on_char  ' ' str in
  let new_str_as_list = remove_empty_spaces str_as_list [] in
  let new_str_as_list = extract_commas new_str_as_list [] in
  let rec build_player_info (build_info:string list)
      (acc:Model.player_info list) : (Model.player_info list) * bool =
    match build_info with
    | [] -> if (List.length acc) = 0 then raise Empty else acc,false
    | "open"::[] -> if (List.length acc) = 0 then raise Empty else acc,true
    | name::"human"::chips::","::t ->
      build_player_info t (
        (name,(convert_string_int_instantiation chips),Human)::acc)
    | name::"h"::chips::","::t ->
      build_player_info t (
        (name,(convert_string_int_instantiation chips),Human)::acc)
    | name::"ai"::chips::","::t ->
      build_player_info t (
        (name,(convert_string_int_instantiation chips),AI (0,0))::acc)
    | name::"a"::chips::","::t ->
      build_player_info t (
        (name,(convert_string_int_instantiation chips),AI (0,0))::acc)
    | name::"ai"::chips::aggression::skill::","::t ->
      let a = convert_string_int_instantiation aggression in
      let s = convert_string_int_instantiation skill in
      build_player_info t (
        (name,(convert_string_int_instantiation chips),AI (a,s))::acc)
    | name::"a"::chips::aggression::skill::","::t ->
      let a = convert_string_int_instantiation aggression in
      let s = convert_string_int_instantiation skill in
      build_player_info t (
        (name,(convert_string_int_instantiation chips),AI (a,s))::acc)
    (*Special instantiations*)
    | ["basic"] ->
      [("Al",50,Human);("Bo",50,Human);("Cam",50,Human)],false
    | ["basic-ai"] ->
      [("Al",50,AI (0,0));("Bo",50,AI (0,0));("Cam",50,AI (0,0))],true
    | ["load"; i] -> raise (Retrieve i)
    | ["retrieve"; i] -> raise (Retrieve i)
    | ["exit"] -> raise PrematureExit
    | _ -> raise Malformed
  in build_player_info new_str_as_list []
