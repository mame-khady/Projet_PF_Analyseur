(*Exercice 3.3 *)

type 'a lazy_list =
  | LNil
  | LCons of 'a * 'a lazy_list Lazy.t

(* Utilitaires de base *)
let rec lazy_of_string s i =
  if i >= String.length s then LNil
  else LCons (s.[i], lazy (lazy_of_string s (i + 1)))

let string_to_lazy s = lazy_of_string s 0

(* Type du résultat de parsing : une liste paresseuse de (valeur, reste) *)
type ('a, 'b) parse_result = ('a * 'b lazy_list) lazy_list

(* Type du parseur *)
type ('a, 'b) parser = 'b lazy_list -> ('a, 'b) parse_result

(* Primitives lazy *)
let rec lazy_append ll1 ll2 =
  match ll1 with
  | LNil -> ll2
  | LCons (x, rest) -> LCons (x, lazy (lazy_append (Lazy.force rest) ll2))

let rec lazy_map f ll =
  match ll with
  | LNil -> LNil
  | LCons (x, rest) -> LCons (f x, lazy (lazy_map f (Lazy.force rest)))

let rec lazy_concat lll =
  match lll with
  | LNil -> LNil
  | LCons (ll, rest) -> lazy_append ll (lazy_concat (Lazy.force rest))


(* Réussite immédiate *)
let return v : ('a, 'b) parser = fun input ->
  LCons ((v, input), lazy LNil)

(* Échec *)
let fail : ('a, 'b) parser = fun _ -> LNil

(* bind (>>=) : pour enchainer les calculs et récupérer les variables *)
let ( >>= ) (p : ('a, 'b) parser) (f : 'a -> ('c, 'b) parser) : ('c, 'b) parser =
  fun input ->
    let results = p input in
    lazy_concat (lazy_map (fun (v, rest) -> (f v) rest) results)

(* Choix (++>) : essaie p1, puis p2 *)
let ( ++> ) (p1 : ('a, 'b) parser) (p2 : ('a, 'b) parser) : ('a, 'b) parser =
  fun input -> lazy_append (p1 input) (p2 input)

(* Terminal qui satisfait un prédicat *)
let satisfy (predicate : 'b -> bool) : ('b, 'b) parser = fun input ->
  match input with
  | LCons (x, rest) when predicate x -> LCons ((x, Lazy.force rest), lazy LNil)
  | _ -> LNil

type token =
  | TInt of int | TIdent of string | TPlus | TMinus | TMult | TDiv
  | TLParen | TRParen | TAssign | TSemicolon | TIf | TThen | TElse
  | TWhile | TDo | TSkip | TTrue | TFalse | TNot | TAnd | TOr
  | TEq | TNeq | TLt | TLe | TGt | TGe | TEOF

(* Utilitaires char *)
let is_digit c = c >= '0' && c <= '9'
let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alphanum c = is_alpha c || is_digit c || c = '_'
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let char_to_int c = int_of_char c - int_of_char '0'

(* Lecture paresseuse d'un entier *)
let rec read_int_lazy acc input =
  match input with
  | LCons (c, rest) when is_digit c ->
      read_int_lazy (acc * 10 + char_to_int c) (Lazy.force rest)
  | _ -> (acc, input)

(* Lecture paresseuse d'un identifiant *)
let rec read_ident_lazy acc input =
  match input with
  | LCons (c, rest) when is_alphanum c ->
      read_ident_lazy (c :: acc) (Lazy.force rest)
  | _ -> (List.rev acc, input)

(* Mapping String -> Token *)
let string_of_chars l = String.of_seq (List.to_seq l)

let ident_to_token s =
  match s with
  | "if" -> TIf | "then" -> TThen | "else" -> TElse | "while" -> TWhile
  | "do" -> TDo | "skip" -> TSkip | "true" -> TTrue | "false" -> TFalse
  | "not" -> TNot | "and" -> TAnd | "or" -> TOr | _ -> TIdent s

(* Le Lexer Principal : char lazy_list -> token lazy_list *)
let rec lex_lazy input : token lazy_list =
  match input with
  | LNil -> LNil
  | LCons (c, rest) when is_space c -> lex_lazy (Lazy.force rest)
  | LCons ('+', rest) -> LCons (TPlus, lazy (lex_lazy (Lazy.force rest)))
  | LCons ('-', rest) -> LCons (TMinus, lazy (lex_lazy (Lazy.force rest)))
  | LCons ('*', rest) -> LCons (TMult, lazy (lex_lazy (Lazy.force rest)))
  | LCons ('/', rest) -> LCons (TDiv, lazy (lex_lazy (Lazy.force rest)))
  | LCons ('(', rest) -> LCons (TLParen, lazy (lex_lazy (Lazy.force rest)))
  | LCons (')', rest) -> LCons (TRParen, lazy (lex_lazy (Lazy.force rest)))
  | LCons (';', rest) -> LCons (TSemicolon, lazy (lex_lazy (Lazy.force rest)))
  | LCons (':', rest) ->
      (match Lazy.force rest with
       | LCons ('=', rest2) -> LCons (TAssign, lazy (lex_lazy (Lazy.force rest2)))
       | _ -> failwith "Lexing error: expected = after :")
  | LCons ('<', rest) ->
      (match Lazy.force rest with
       | LCons ('>', rest2) -> LCons (TNeq, lazy (lex_lazy (Lazy.force rest2)))
       | LCons ('=', rest2) -> LCons (TLe, lazy (lex_lazy (Lazy.force rest2)))
       | _ -> LCons (TLt, lazy (lex_lazy (Lazy.force rest))))
  | LCons ('>', rest) ->
      (match Lazy.force rest with
       | LCons ('=', rest2) -> LCons (TGe, lazy (lex_lazy (Lazy.force rest2)))
       | _ -> LCons (TGt, lazy (lex_lazy (Lazy.force rest))))
  | LCons ('=', rest) -> LCons (TEq, lazy (lex_lazy (Lazy.force rest)))
  | LCons (c, rest) when is_digit c ->
      (* Attention : on doit repasser 'input' complet à read_int_lazy *)
      let (n, rest_after_int) = read_int_lazy 0 input in
      LCons (TInt n, lazy (lex_lazy rest_after_int))
  | LCons (c, rest) when is_alpha c ->
      let (chars, rest_after_ident) = read_ident_lazy [] input in
      let token = ident_to_token (string_of_chars chars) in
      LCons (token, lazy (lex_lazy rest_after_ident))
  | LCons (c, _) -> failwith ("Unexpected char: " ^ String.make 1 c)


type aexp =
  | AInt of int | AVar of string | APlus of aexp * aexp | AMinus of aexp * aexp
  | AMult of aexp * aexp | ADiv of aexp * aexp

type bexp =
  | BTrue | BFalse | BNot of bexp | BAnd of bexp * bexp | BOr of bexp * bexp
  | BEq of aexp * aexp | BNeq of aexp * aexp | BLt of aexp * aexp
  | BLe of aexp * aexp | BGt of aexp * aexp | BGe of aexp * aexp

type instr =
  | ISkip | IAssign of string * aexp | ISeq of instr * instr
  | IIf of bexp * instr * instr | IWhile of bexp * instr

(* Primitives de parsing sur les tokens *)
let token t = satisfy (fun x -> x = t)

let p_int = fun input ->
  match input with
  | LCons (TInt n, rest) -> LCons ((n, Lazy.force rest), lazy LNil)
  | _ -> LNil

let p_ident = fun input ->
  match input with
  | LCons (TIdent s, rest) -> LCons ((s, Lazy.force rest), lazy LNil)
  | _ -> LNil

(* --- Expressions Arithmétiques --- *)

let rec p_aexp input = p_aexp_add input

and p_aexp_add input =
  ( (p_aexp_mult >>= fun e1 ->
     token TPlus >>= fun _ ->
     p_aexp_add >>= fun e2 ->
     return (APlus (e1, e2)))
    ++>
    (p_aexp_mult >>= fun e1 ->
     token TMinus >>= fun _ ->
     p_aexp_add >>= fun e2 ->
     return (AMinus (e1, e2)))
    ++>
    p_aexp_mult
  ) input

and p_aexp_mult input =
  ( (p_aexp_atom >>= fun e1 ->
     token TMult >>= fun _ ->
     p_aexp_mult >>= fun e2 ->
     return (AMult (e1, e2)))
    ++>
    (p_aexp_atom >>= fun e1 ->
     token TDiv >>= fun _ ->
     p_aexp_mult >>= fun e2 ->
     return (ADiv (e1, e2)))
    ++>
    p_aexp_atom
  ) input

and p_aexp_atom input =
  ( (p_int >>= fun n -> return (AInt n))
    ++>
    (p_ident >>= fun x -> return (AVar x))
    ++>
    (token TLParen >>= fun _ ->
     p_aexp >>= fun e ->
     token TRParen >>= fun _ ->
     return e)
  ) input

(* --- Expressions Booléennes (Simplifié) --- *)

let rec p_bexp input =
    (* On simplifie ici pour l'exemple, similaire à aexp *)
    ((token TTrue >>= fun _ -> return BTrue)
    ++>
    (token TFalse >>= fun _ -> return BFalse)
    ++>
    (p_aexp >>= fun e1 ->
     token TEq >>= fun _ ->
     p_aexp >>= fun e2 -> return (BEq(e1, e2)))
    ++>
    (token TNot >>= fun _ ->
     p_bexp >>= fun b -> return (BNot b))
    ) input

(* --- Instructions --- *)

let rec p_instr input = p_seq input

and p_seq input =
  ( (p_atom >>= fun i1 ->
     token TSemicolon >>= fun _ ->
     p_seq >>= fun i2 ->
     return (ISeq (i1, i2)))
    ++>
    p_atom
  ) input

and p_atom input =
  ( (token TSkip >>= fun _ -> return ISkip)
    ++>
    (p_ident >>= fun x ->
     token TAssign >>= fun _ ->
     p_aexp >>= fun e ->
     return (IAssign (x, e)))
    ++>
    (token TWhile >>= fun _ ->
     p_bexp >>= fun b ->
     token TDo >>= fun _ ->
     p_instr >>= fun i ->
     return (IWhile (b, i)))
    ++>
    (token TIf >>= fun _ ->
     p_bexp >>= fun b ->
     token TThen >>= fun _ ->
     p_instr >>= fun i1 ->
     token TElse >>= fun _ ->
     p_instr >>= fun i2 ->
     return (IIf(b, i1, i2)))
  ) input


let parse_string str =
  let char_stream = string_to_lazy str in
  let token_stream = lex_lazy char_stream in
  let results = p_instr token_stream in
  match results with
  | LCons ((ast, rest), _) ->
      (* On vérifie si tout a été consommé (rest est vide ou EOF) *)
      (* Note: ici on prend le premier résultat valide *)
      print_endline "Parsing réussi !";
      ast
  | LNil -> failwith "Erreur de syntaxe"

(* Test *)
let () =
  print_endline "\n=== Test Parsing Lazy ===";
  let ast = parse_string "x := 42; while true do x := x + 1" in
  print_endline "AST généré avec succès."