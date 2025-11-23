(*3.2 Analyse lexicale et syntaxique*)

(* Types pour les tokens *)
type token =
  | TInt of int
  | TIdent of string
  | TPlus
  | TMinus
  | TMult
  | TDiv
  | TLParen
  | TRParen
  | TAssign    (* := *)
  | TSemicolon (* ; *)
  | TIf
  | TThen
  | TElse
  | TWhile
  | TDo
  | TSkip
  | TTrue
  | TFalse
  | TNot
  | TAnd
  | TOr
  | TEq        (* = *)
  | TNeq       (* <> *)
  | TLt        (* < *)
  | TLe        (* <= *)
  | TGt        (* > *)
  | TGe        (* >= *)
  | TEOF

(* Utilitaire pour les caractères *)
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alphanum c = is_alpha c || is_digit c || c = '_'
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let char_to_int c = int_of_char c - int_of_char '0'

(* Convertir une string en liste de caractères *)
let string_to_list s =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (s.[i] :: acc)
  in
  aux (String.length s - 1) []

(* Convertir une liste de caractères en string *)
let list_to_string l =
  let s = Bytes.create (List.length l) in
  let rec aux i = function
    | [] -> Bytes.to_string s
    | c :: rest -> Bytes.set s i c; aux (i + 1) rest
  in
  aux 0 l

(* Lire un entier avec le schéma de Horner *)
let rec read_int acc = function
  | c :: rest when is_digit c ->
      read_int (acc * 10 + char_to_int c) rest
  | rest -> (acc, rest)

(* Lire un identifiant ou mot-clé *)
let rec read_ident acc = function
  | c :: rest when is_alphanum c ->
      read_ident (c :: acc) rest
  | rest -> (List.rev acc, rest)

(* Convertir identifiant en token (gestion des mots-clés) *)
let ident_to_token s =
  match s with
  | "if" -> TIf
  | "then" -> TThen
  | "else" -> TElse
  | "while" -> TWhile
  | "do" -> TDo
  | "skip" -> TSkip
  | "true" -> TTrue
  | "false" -> TFalse
  | "not" -> TNot
  | "and" -> TAnd
  | "or" -> TOr
  | _ -> TIdent s

(* Sauter les espaces et retours à la ligne *)
let rec skip_spaces = function
  | c :: rest when is_space c -> skip_spaces rest
  | rest -> rest

(* Analyseur lexical principal : retourne (token option, reste) *)
let next_token input =
  let input = skip_spaces input in
  match input with
  | [] -> (None, [])
  | '+' :: rest -> (Some TPlus, rest)
  | '-' :: rest -> (Some TMinus, rest)
  | '*' :: rest -> (Some TMult, rest)
  | '/' :: rest -> (Some TDiv, rest)
  | '(' :: rest -> (Some TLParen, rest)
  | ')' :: rest -> (Some TRParen, rest)
  | ';' :: rest -> (Some TSemicolon, rest)
  | ':' :: '=' :: rest -> (Some TAssign, rest)
  | '<' :: '>' :: rest -> (Some TNeq, rest)
  | '<' :: '=' :: rest -> (Some TLe, rest)
  | '<' :: rest -> (Some TLt, rest)
  | '>' :: '=' :: rest -> (Some TGe, rest)
  | '>' :: rest -> (Some TGt, rest)
  | '=' :: rest -> (Some TEq, rest)
  | c :: rest when is_digit c ->
      let (n, rest') = read_int 0 input in
      (Some (TInt n), rest')
  | c :: rest when is_alpha c ->
      let (ident_chars, rest') = read_ident [] input in
      let ident = list_to_string ident_chars in
      (Some (ident_to_token ident), rest')
  | c :: rest -> 
    failwith ("Unexpected character: " ^ String.make 1 c)


(* Produire la liste de tous les tokens *)
let tokenize input =
  let rec aux acc input =
    match next_token input with
    | (None, _) -> List.rev acc
    | (Some tok, rest) -> aux (tok :: acc) rest
  in
  aux [] (string_to_list input)

(* Version pratique depuis une string *)
let lex s = tokenize s

(* Fonction d'affichage pour debug *)
let string_of_token = function
  | TInt n -> "INT(" ^ string_of_int n ^ ")"
  | TIdent s -> "IDENT(" ^ s ^ ")"
  | TPlus -> "+"
  | TMinus -> "-"
  | TMult -> "*"
  | TDiv -> "/"
  | TLParen -> "("
  | TRParen -> ")"
  | TAssign -> ":="
  | TSemicolon -> ";"
  | TIf -> "if"
  | TThen -> "then"
  | TElse -> "else"
  | TWhile -> "while"
  | TDo -> "do"
  | TSkip -> "skip"
  | TTrue -> "true"
  | TFalse -> "false"
  | TNot -> "not"
  | TAnd -> "and"
  | TOr -> "or"
  | TEq -> "="
  | TNeq -> "<>"
  | TLt -> "<"
  | TLe -> "<="
  | TGt -> ">"
  | TGe -> ">="
  | TEOF -> "EOF"

let print_tokens tokens =
  List.iter (fun t -> print_endline (string_of_token t)) tokens

(* Tests *)
let () =
  print_endline "=== Test 1: Expression simple ===";
  print_tokens (lex "x := 42 + 3");
  
  print_endline "\n=== Test 2: Boucle while ===";
  print_tokens (lex "while x > 0 do x := x - 1");
  
  print_endline "\n=== Test 3: If-then-else ===";
  print_tokens (lex "if x = 0 then skip else x := 1")

(* AST simplifié *)
type aexp =
  | AInt of int
  | AVar of string
  | APlus of aexp * aexp
  | AMinus of aexp * aexp
  | AMult of aexp * aexp
  | ADiv of aexp * aexp

type bexp =
  | BTrue
  | BFalse
  | BNot of bexp
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | BEq of aexp * aexp
  | BNeq of aexp * aexp
  | BLt of aexp * aexp
  | BLe of aexp * aexp
  | BGt of aexp * aexp   
  | BGe of aexp * aexp          

type instr =
  | ISkip
  | IAssign of string * aexp
  | ISeq of instr * instr
  | IIf of bexp * instr * instr
  | IWhile of bexp * instr

(* Analyseur syntaxique avec descente récursive *)
exception Parse_error of string

let parse_error msg = raise (Parse_error msg)

(* Analyseur d'expressions arithmétiques *)
let rec parse_aexp tokens =
  parse_aexp_add tokens

and parse_aexp_add tokens =
  let (e1, tokens') = parse_aexp_mult tokens in
  match tokens' with
  | TPlus :: rest ->
      let (e2, tokens'') = parse_aexp_add rest in
      (APlus (e1, e2), tokens'')
  | TMinus :: rest ->
      let (e2, tokens'') = parse_aexp_add rest in
      (AMinus (e1, e2), tokens'')
  | _ -> (e1, tokens')

and parse_aexp_mult tokens =
  let (e1, tokens') = parse_aexp_atom tokens in
  match tokens' with
  | TMult :: rest ->
      let (e2, tokens'') = parse_aexp_mult rest in
      (AMult (e1, e2), tokens'')
  | TDiv :: rest ->
      let (e2, tokens'') = parse_aexp_mult rest in
      (ADiv (e1, e2), tokens'')
  | _ -> (e1, tokens')

and parse_aexp_atom tokens =
  match tokens with
  | TInt n :: rest -> (AInt n, rest)
  | TIdent x :: rest -> (AVar x, rest)
  | TLParen :: rest ->
      let (e, tokens') = parse_aexp rest in
      (match tokens' with
       | TRParen :: rest' -> (e, rest')
       | _ -> parse_error "Expected ')'")
  | _ -> parse_error "Expected expression"

(* Analyseur d'expressions booléennes *)
let rec parse_bexp tokens =
  parse_bexp_or tokens

and parse_bexp_or tokens =
  let (e1, tokens') = parse_bexp_and tokens in
  match tokens' with
  | TOr :: rest ->
      let (e2, tokens'') = parse_bexp_or rest in
      (BOr (e1, e2), tokens'')
  | _ -> (e1, tokens')

and parse_bexp_and tokens =
  let (e1, tokens') = parse_bexp_not tokens in
  match tokens' with
  | TAnd :: rest ->
      let (e2, tokens'') = parse_bexp_and rest in
      (BAnd (e1, e2), tokens'')
  | _ -> (e1, tokens')

and parse_bexp_not tokens =
  match tokens with
  | TNot :: rest ->
      let (e, tokens') = parse_bexp_not rest in
      (BNot e, tokens')
  | _ -> parse_bexp_atom tokens

and parse_bexp_atom tokens =
  match tokens with
  | TTrue :: rest -> (BTrue, rest)
  | TFalse :: rest -> (BFalse, rest)
  | TLParen :: rest ->
      let (e, tokens') = parse_bexp rest in
      (match tokens' with
       | TRParen :: rest' -> (e, rest')
       | _ -> parse_error "Expected ')'")
  | _ ->
      (* Essayer de parser une comparaison *)
      let (e1, tokens') = parse_aexp tokens in
      (match tokens' with
       | TEq :: rest ->
           let (e2, tokens'') = parse_aexp rest in
           (BEq (e1, e2), tokens'')
       | TNeq :: rest ->
           let (e2, tokens'') = parse_aexp rest in
           (BNeq (e1, e2), tokens'')
       | TLt :: rest ->
           let (e2, tokens'') = parse_aexp rest in
           (BLt (e1, e2), tokens'')
       | TLe :: rest ->
           let (e2, tokens'') = parse_aexp rest in
           (BLe (e1, e2), tokens'')
       | _ -> parse_error "Expected boolean expression")

(* Analyseur d'instructions *)
let rec parse_instr tokens =
  parse_instr_seq tokens

and parse_instr_seq tokens =
  let (i1, tokens') = parse_instr_atom tokens in
  match tokens' with
  | TSemicolon :: rest ->
      let (i2, tokens'') = parse_instr_seq rest in
      (ISeq (i1, i2), tokens'')
  | _ -> (i1, tokens')

and parse_instr_atom tokens =
  match tokens with
  | TSkip :: rest -> (ISkip, rest)
  | TIdent x :: TAssign :: rest ->
      let (e, tokens') = parse_aexp rest in
      (IAssign (x, e), tokens')
  | TIf :: rest ->
      let (b, tokens') = parse_bexp rest in
      (match tokens' with
       | TThen :: rest' ->
           let (i1, tokens'') = parse_instr rest' in
           (match tokens'' with
            | TElse :: rest'' ->
                let (i2, tokens''') = parse_instr rest'' in
                (IIf (b, i1, i2), tokens''')
            | _ -> parse_error "Expected 'else'")
       | _ -> parse_error "Expected 'then'")
  | TWhile :: rest ->
      let (b, tokens') = parse_bexp rest in
      (match tokens' with
       | TDo :: rest' ->
           let (i, tokens'') = parse_instr rest' in
           (IWhile (b, i), tokens'')
       | _ -> parse_error "Expected 'do'")
  | TLParen :: rest ->
      let (i, tokens') = parse_instr rest in
      (match tokens' with
       | TRParen :: rest' -> (i, rest')
       | _ -> parse_error "Expected ')'")
  | _ -> parse_error "Expected instruction"

(* Fonction principale de parsing *)
let parse s =
  let tokens = lex s in
  let (ast, rest) = parse_instr tokens in
  if rest <> [] then
    parse_error ("Unexpected tokens at end: " ^ 
                 String.concat " " (List.map string_of_token rest))
  else
    ast

let () =
  print_endline "\n=== Test parsing ===";
  
  try
    let ast1 = parse "x := 42" in
    print_endline "Parse successful: x := 42";
    
    let ast2 = parse "while x > 0 do x := x - 1" in
    print_endline "Parse successful: while x > 0 do x := x - 1";
    
    let ast3 = parse "if x = 0 then skip else x := 1" in
    print_endline "Parse successful: if x = 0 then skip else x := 1";
    
    let ast4 = parse "x := 0; y := 1; while not (x = 10) do (x := x + 1; y := y * 2)" in
    print_endline "Parse successful: complex program";
  with
  | Parse_error msg -> print_endline ("Parse error: " ^ msg)
(*==============================================================================*)                         
(*3.3 Liste paresseuses*)

(* Type pour les listes paresseuses *)
(* Type pour les listes paresseuses *)
type 'a lazy_list =
  | LNil
  | LCons of 'a * 'a lazy_list Lazy.t

(* Type pour les résultats de parsing *)
type ('a, 'b) parse_result = ('a * 'b lazy_list) lazy_list

(* Un parser est une fonction qui prend une lazy_list et retourne des résultats *)
type ('a, 'b) parser = 'b lazy_list -> ('a, 'b) parse_result

(* Conversion string vers lazy_list *)
let rec lazy_of_string s i =
  if i >= String.length s then LNil
  else LCons (s.[i], lazy (lazy_of_string s (i + 1)))

let lazy_of_string s = lazy_of_string s 0

(* Conversion list vers lazy_list *)
let rec lazy_of_list = function
  | [] -> LNil
  | x :: xs -> LCons (x, lazy (lazy_of_list xs))

(* Map sur lazy_list *)
let rec lazy_map f ll =
  match ll with
  | LNil -> LNil
  | LCons (x, rest) -> LCons (f x, lazy (lazy_map f (Lazy.force rest)))

(* Append sur lazy_list *)
let rec lazy_append ll1 ll2 =
  match ll1 with
  | LNil -> ll2
  | LCons (x, rest) -> LCons (x, lazy (lazy_append (Lazy.force rest) ll2))

(* Concat sur lazy_list de lazy_list *)
let rec lazy_concat lll =
  match lll with
  | LNil -> LNil
  | LCons (ll, rest) -> lazy_append ll (lazy_concat (Lazy.force rest))


(* terminal : reconnait un token spécifique *)
let terminal t : ('a, 'a) parser = fun input ->
  match input with
  | LNil -> LNil
  | LCons (x, rest) when x = t -> 
      LCons ((x, Lazy.force rest), lazy LNil)
  | _ -> LNil

(* epsilon : reconnait la chaîne vide *)
let epsilon : (unit, 'b) parser = fun input ->
  LCons (((), input), lazy LNil)

(* epsilon_res : reconnait la chaîne vide et retourne une valeur *)
let epsilon_res v : ('a, 'b) parser = fun input ->
  LCons ((v, input), lazy LNil)

(* (-->) : séquencement de parseurs (puis) *)
let (-->) (p1 : ('a, 'c) parser) (p2 : ('b, 'c) parser) : ('a * 'b, 'c) parser =
  fun input ->
    let results1 = p1 input in
    lazy_concat (lazy_map (fun (v1, rest1) ->
      let results2 = p2 rest1 in
      lazy_map (fun (v2, rest2) -> ((v1, v2), rest2)) results2
    ) results1)

(* (++>) : choix entre parseurs (ou) *)
let (++>) (p1 : ('a, 'b) parser) (p2 : ('a, 'b) parser) : ('a, 'b) parser =
  fun input ->
  lazy_append (p1 input) (p2 input)

(*============================================================================*)
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

(* Implémentation avec association list (immutable) *)
module StateAssoc : STATE = struct
  type t = (string * int) list
  
  let init () = []
  
  let rec get state var =
    match List.assoc_opt var state with
    | Some v -> v
    | None -> 0  (* Variables non initialisées valent 0 *)
  
  let set state var value =
    (* Créer un nouvel état avec la valeur mise à jour *)
    let rec update acc = function
      | [] -> List.rev ((var, value) :: acc)
      | (v, n) :: rest when v = var -> 
          List.rev_append acc ((var, value) :: rest)
      | x :: rest -> update (x :: acc) rest
    in
    update [] state
  
  let copy state = state  (* Immutable, donc copie = identité *)
  
  let to_string state =
    let bindings = List.map (fun (v, n) -> v ^ "=" ^ string_of_int n) state in
    "[" ^ String.concat "; " bindings ^ "]"
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

(* Implémentation avec Hashtbl (mutable - DANGER!) *)
module StateMutable : STATE = struct
  type t = (string, int) Hashtbl.t
  
  let init () = Hashtbl.create 10
  
  let get state var =
    try Hashtbl.find state var
    with Not_found -> 0
  
  let set state var value =
    Hashtbl.replace state var value;
    state  (* Retourne l'état modifié *)
  
  let copy state =
    Hashtbl.copy state
  
  let to_string state =
    let bindings = Hashtbl.fold (fun k v acc -> (k, v) :: acc) state [] in
    let strs = List.map (fun (v, n) -> v ^ "=" ^ string_of_int n) bindings in
    "[" ^ String.concat "; " strs ^ "]"
end

(* Choisir l'implémentation *)
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

(*===============================================================================*)
    
(*3.5 Interpréteur amélioré*)
    (* Exécuter avec compteur de pas *)
let executer_compte prog =
  let rec aux config steps =
    match config with
    | Final s -> (s, steps)
    | Inter _ -> aux (faire_un_pas config) (steps + 1)
  in
  let (final_state, nb_steps) = aux (Inter (prog, State.init ())) 0 in
  Printf.printf "Programme exécuté en %d pas\n" nb_steps;
  final_state

(* Version avec affichage de chaque pas *)
let executer_verbose prog =
  let rec aux config steps =
    Printf.printf "\n=== Pas %d ===\n" steps;
    (match config with
     | Final s -> 
         Printf.printf "Configuration finale: %s\n" (State.to_string s);
         (s, steps)
     | Inter (i, s) ->
         Printf.printf "État: %s\n" (State.to_string s);
         aux (faire_un_pas config) (steps + 1))
  in
  let (final_state, nb_steps) = aux (Inter (prog, State.init ())) 0 in
  Printf.printf "\nTotal: %d pas\n" nb_steps;
  final_state

(* Afficher une instruction de manière lisible *)
let rec string_of_instr = function
  | ISkip -> "skip"
  | IAssign (x, e) -> x ^ " := " ^ string_of_aexp e
  | ISeq (i1, i2) -> string_of_instr i1 ^ "; " ^ string_of_instr i2
  | IIf (b, i1, i2) -> 
      "if " ^ string_of_bexp b ^ " then " ^ 
      string_of_instr i1 ^ " else " ^ string_of_instr i2
  | IWhile (b, i) -> 
      "while " ^ string_of_bexp b ^ " do " ^ string_of_instr i

and string_of_aexp = function
  | AInt n -> string_of_int n
  | AVar x -> x
  | APlus (e1, e2) -> "(" ^ string_of_aexp e1 ^ " + " ^ string_of_aexp e2 ^ ")"
  | AMinus (e1, e2) -> "(" ^ string_of_aexp e1 ^ " - " ^ string_of_aexp e2 ^ ")"
  | AMult (e1, e2) -> "(" ^ string_of_aexp e1 ^ " * " ^ string_of_aexp e2 ^ ")"
  | ADiv (e1, e2) -> "(" ^ string_of_aexp e1 ^ " / " ^ string_of_aexp e2 ^ ")"

and string_of_bexp = function
  | BTrue -> "true"
  | BFalse -> "false"
  | BNot b -> "not " ^ string_of_bexp b
  | BAnd (b1, b2) -> "(" ^ string_of_bexp b1 ^ " and " ^ string_of_bexp b2 ^ ")"
  | BOr (b1, b2) -> "(" ^ string_of_bexp b1 ^ " or " ^ string_of_bexp b2 ^ ")"
  | BEq (e1, e2) -> string_of_aexp e1 ^ " = " ^ string_of_aexp e2
  | BNeq (e1, e2) -> string_of_aexp e1 ^ " <> " ^ string_of_aexp e2
  | BLt (e1, e2) -> string_of_aexp e1 ^ " < " ^ string_of_aexp e2
  | BLe (e1, e2) -> string_of_aexp e1 ^ " <= " ^ string_of_aexp e2
  | BGt (e1, e2) -> string_of_aexp e1 ^ " > " ^ string_of_aexp e2
  | BGe (e1, e2) -> string_of_aexp e1 ^ " >= " ^ string_of_aexp e2

(* Mode interactif pas à pas *)
let executer_interactif prog =
  let rec aux config steps =
    Printf.printf "\n========================================\n";
    Printf.printf "Pas %d\n" steps;
    Printf.printf "========================================\n";
    
    match config with
    | Final s -> 
        Printf.printf "CONFIGURATION FINALE\n";
        Printf.printf "État final: %s\n" (State.to_string s);
        s
    
    | Inter (i, s) ->
        Printf.printf "Instruction à exécuter:\n  %s\n" (string_of_instr i);
        Printf.printf "État courant: %s\n" (State.to_string s);
        Printf.printf "\nOptions:\n";
        Printf.printf "  [Entrée] - Exécuter un pas\n";
        Printf.printf "  c - Continuer jusqu'à la fin\n";
        Printf.printf "  n N - Exécuter N pas\n";
        Printf.printf "  q - Quitter\n";
        Printf.printf "Votre choix: ";
        flush stdout;
        
        let input = read_line () in
        match String.trim input with
        | "" -> 
            (* Un pas *)
            aux (faire_un_pas config) (steps + 1)
        
        | "c" -> 
            (* Continuer jusqu'à la fin *)
            let rec run_to_end cfg st =
              match cfg with
              | Final s -> s
              | Inter _ -> run_to_end (faire_un_pas cfg) (st + 1)
            in
            run_to_end config steps
        
        | "q" -> 
            Printf.printf "Exécution interrompue.\n";
            s
        
        | cmd when String.length cmd > 2 && String.sub cmd 0 2 = "n " ->
            let n = int_of_string (String.sub cmd 2 (String.length cmd - 2)) in
            let rec run_n cfg st count =
              if count = 0 then aux cfg st
              else
                match cfg with
                | Final s -> s
                | Inter _ -> run_n (faire_un_pas cfg) (st + 1) (count - 1)
            in
            run_n config steps n
        
        | _ ->
            Printf.printf "Commande non reconnue.\n";
            aux config steps
  in
  aux (Inter (prog, State.init ())) 0

(*Tests*)
(* Programme exemple: calcul de factorielle *)
let prog_fact =
  ISeq (
    IAssign ("n", AInt 5),
    ISeq (
      IAssign ("result", AInt 1),
      IWhile (
        BGt (AVar "n", AInt 0),
        ISeq (
          IAssign ("result", AMult (AVar "result", AVar "n")),
          IAssign ("n", AMinus (AVar "n", AInt 1))
        )
      )
    )
  )

(* Programme qui boucle *)
let prog_boucle_infinie =
  IWhile (BTrue, IAssign ("x", APlus (AVar "x", AInt 1)))

let () =
  print_endline "=== Test 1: Exécution normale ===";
  let s1 = executer prog_fact in
  Printf.printf "Résultat: %s\n" (State.to_string s1);
  
  print_endline "\n=== Test 2: Exécution avec compteur ===";
  let s2 = executer_compte prog_fact in
  Printf.printf "Résultat: %s\n" (State.to_string s2);
  
  print_endline "\n=== Test 3: Exécution avec limite ===";
  (try
    let s3 = executer_limite prog_boucle_infinie 100 in
    Printf.printf "Résultat: %s\n" (State.to_string s3)
  with Failure msg -> Printf.printf "Erreur: %s\n" msg);
  
  print_endline "\n=== Test 4: Mode interactif ===";
  Printf.printf "Tapez 'c' pour passer ce test\n";
  let _ = executer_interactif prog_fact in
  ()


(*==================================================================================*)