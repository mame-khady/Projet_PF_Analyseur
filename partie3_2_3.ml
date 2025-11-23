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
