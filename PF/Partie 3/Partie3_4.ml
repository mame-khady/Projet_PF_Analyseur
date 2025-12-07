(*3.4 Mécanique d’état et interpréteur*)


(* Module signature pour l'état *)
module type STATE = sig
  type t
  val init : unit -> t
  val get : t -> string -> int
  val set : t -> string -> int -> t
  val copy : t -> t
  val to_string : t -> string
end

(* Implémentation avec Map (immutable, plus efficace) *)
module StateMap : STATE = struct
  module SMap = Map.Make(String)

  type t = int SMap.t

  let init () = SMap.empty

  let get state var =
    try SMap.find var state
    with Not_found -> 0

  let set state var value =
    SMap.add var value state

  let copy state = state

  let to_string state =
    let bindings = SMap.bindings state in
    let strs = List.map (fun (v, n) -> v ^ "=" ^ string_of_int n) bindings in
    "[" ^ String.concat "; " strs ^ "]"
end

module State = StateMap

(* Évaluation des expressions arithmétiques *)
let rec eval_aexp (state : State.t) = function
  | AInt n -> n
  | AVar x -> State.get state x
  | APlus (e1, e2) -> eval_aexp state e1 + eval_aexp state e2
  | AMinus (e1, e2) -> eval_aexp state e1 - eval_aexp state e2
  | AMult (e1, e2) -> eval_aexp state e1 * eval_aexp state e2
  | ADiv (e1, e2) ->
      let v2 = eval_aexp state e2 in
      if v2 = 0 then failwith "Division by zero"
      else eval_aexp state e1 / v2

(* Évaluation des expressions booléennes *)
let rec eval_bexp (state : State.t) = function
  | BTrue -> true
  | BFalse -> false
  | BNot b -> not (eval_bexp state b)
  | BAnd (b1, b2) -> eval_bexp state b1 && eval_bexp state b2
  | BOr (b1, b2) -> eval_bexp state b1 || eval_bexp state b2
  | BEq (e1, e2) -> eval_aexp state e1 = eval_aexp state e2
  | BNeq (e1, e2) -> eval_aexp state e1 <> eval_aexp state e2
  | BLt (e1, e2) -> eval_aexp state e1 < eval_aexp state e2
  | BLe (e1, e2) -> eval_aexp state e1 <= eval_aexp state e2
  | BGt (e1, e2) -> eval_aexp state e1 > eval_aexp state e2
  | BGe (e1, e2) -> eval_aexp state e1 >= eval_aexp state e2

(* Exécuter une affectation *)
let exec_assign state var expr =
  let value = eval_aexp state expr in
  State.set state var value


(* Type configuration *)
type config =
  | Inter of instr * State.t
  | Final of State.t

(* Faire un pas de transition selon les règles SOS *)
let rec faire_un_pas = function
  | Final s -> Final s  (* Configuration finale, rien à faire *)
  | Inter (prog, state) ->
      match prog with
      | ISkip ->
          Final state

      | IAssign (x, e) ->
          let new_state = exec_assign state x e in
          Final new_state

      | ISeq (i1, i2) ->
          (match faire_un_pas (Inter (i1, state)) with
           | Final s1 -> Inter (i2, s1)
           | Inter (i1', s1) -> Inter (ISeq (i1', i2), s1))

      | IIf (b, i1, i2) ->
          if eval_bexp state b then
            Inter (i1, state)
          else
            Inter (i2, state)

      | IWhile (b, i) ->
          Inter (IIf (b, ISeq (i, IWhile (b, i)), ISkip), state)

(* Vérifier si une configuration est finale *)
let is_final = function
  | Final _ -> true
  | Inter _ -> false

(* Exécuter un programme jusqu'à la fin *)
let rec executer prog =
  let rec aux config =
    match config with
    | Final s -> s
    | Inter _ -> aux (faire_un_pas config)
  in
  aux (Inter (prog, State.init ()))

(* Version avec limite de pas (pour éviter boucles infinies) *)
let executer_limite prog max_steps =
  let rec aux config steps =
    if steps >= max_steps then
      failwith "Maximum number of steps reached (possible infinite loop)"
    else
      match config with
      | Final s -> s
      | Inter _ -> aux (faire_un_pas config) (steps + 1)
  in
  aux (Inter (prog, State.init ())) 0