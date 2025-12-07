(* ========================================================================== *)
(* EXERCICE 2.1.3 : Expressions Complexes et Priorités                        *)
(* Objectif : Valider ! > . > +                                               *)
(* ========================================================================== *)
#use "anacomb.ml";;

(* 1. TYPES ETENDUS (Exercice 1.1.4) *)

(* 

  C ::= '0' | '1'                       (* Constante : Const *)
  V ::= 'a' | 'b' | 'c' | 'd'           (* Variable : Var *)
  A ::= C | V                            (* Atom : parse_atom *)
  F ::= '!' F | A | '(' E ')'            (* Factor : parse_factor *)
  T ::= F ST                              (* Term : parse_term *)
  ST ::= '.' F ST | epsilon               (* Conjonction : And *)
  E ::= T SE                              (* Expr : parse_expr *)
  SE ::= '+' T SE | epsilon               (* Disjonction : Or *)
 *)


type var = A | B | C | D
(* Ajout de Not, And, Or selon la grammaire *)
type expr = 
  | Const of int 
  | Var of var 
  | Not of expr 
  | And of expr * expr 
  | Or of expr * expr

type prog = 
  | Skip
  | Assign of var * expr
  | Seq of prog * prog
  | If of expr * prog * prog   (* Condition est maintenant une expr *)
  | While of expr * prog       (* Condition est maintenant une expr *)

(* 2. ANALYSEURS LEXICAUX *)
let list_of_string s = 
  let n = String.length s in
  let rec boucle i = if i = n then [] else s.[i] :: boucle (i+1) in 
  boucle 0

let parse_var = terminal_res (function
    | 'a' -> Some A
    | 'b' -> Some B
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)

let parse_digit = terminal_res (function
    | '0' -> Some 0
    | '1' -> Some 1
    | _ -> None)

(* 3. PARSEUR D'EXPRESSIONS (Priorités) *)
(* Hiérarchie : Atom (base) -> Factor (!) -> Term (.) -> Expr (+) *)

let rec parse_atom l = 
  (
    (parse_digit ++> fun n -> epsilon_res (Const n))
    +| 
    (parse_var ++> fun v -> epsilon_res (Var v))
    +| 
    (* Parenthèses : on remonte tout en haut à parse_expr *)
    (terminal '(' -+> parse_expr ++> fun e -> terminal ')' -+> epsilon_res e)
  ) l

and parse_factor l = (* Priorité 1 : Négation '!' *)
  (
    (terminal '!' -+> parse_factor ++> fun e -> epsilon_res (Not e))
    +| parse_atom
  ) l

and parse_term l = (* Priorité 2 : Conjonction '.' *)
  (
    parse_factor ++> fun f -> 
      (
        (terminal '.' -+> parse_term ++> fun t -> epsilon_res (And(f, t)))
        +| epsilon_res f
      )
  ) l

and parse_expr l = (* Priorité 3 : Disjonction '+' *)
  (
    parse_term ++> fun t -> 
      (
        (terminal '+' -+> parse_expr ++> fun e -> epsilon_res (Or(t, e)))
        +| epsilon_res t
      )
  ) l

(* 4. PARSEUR D'INSTRUCTIONS (Mis à jour avec parse_expr) *)

let rec parse_stmt l =
  (
    (* Skip *)
    (terminal 's' --> terminal 'k' --> terminal 'i' --> terminal 'p' -+> epsilon_res Skip)
    +|
    (* Assign : v := expr *)
    (parse_var ++> fun v ->
      terminal ':' --> terminal '=' -+> parse_expr ++> fun e ->
        epsilon_res (Assign(v, e)))
    +|
    (* If : condition complexe autorisée *)
    (terminal 'i' --> terminal '(' -+> parse_expr ++> fun c ->
      terminal ')' --> terminal '{' -+> parse_prog ++> fun p1 ->
        terminal '}' --> terminal '{' -+> parse_prog ++> fun p2 ->
          terminal '}' -+> epsilon_res (If(c, p1, p2)))
    +|
    (* While : condition complexe autorisée *)
    (terminal 'w' --> terminal '(' -+> parse_expr ++> fun c ->
      terminal ')' --> terminal '{' -+> parse_prog ++> fun p ->
        terminal '}' -+> epsilon_res (While(c, p)))
  ) l

and parse_prog l =
  (
    (parse_stmt ++> fun s -> 
      ((terminal ';' -+> parse_prog ++> fun suite -> epsilon_res (Seq(s, suite)))
       +| epsilon_res s)
    )
    +| epsilon_res Skip
  ) l
;;

(* 5. TESTS ET VALIDATION *)

(* Test 1 : Priorité '.' > '+' *)
let r1 = parse_expr (list_of_string "1+0.0") in
assert (r1 = (Or(Const 1, And(Const 0, Const 0)), []));

(* Test 2 : Priorité '!' > '.' *)
let r2 = parse_expr (list_of_string "!1.0") in
assert (r2 = (And(Not(Const 1), Const 0), []));

(* Test 3 : Parenthèses *)
let r3 = parse_expr (list_of_string "!(1+0)") in
assert (r3 = (Not(Or(Const 1, Const 0)), []));

(* Test 4 : Intégration dans If *)
let r4 = parse_prog (list_of_string "i(!a+b){skip}{skip}") in
assert (r4 = (If(Or(Not(Var A), Var B), Skip, Skip), []));

print_endline "Tous les tests des expressions complexes et priorités sont VALIDÉS !";;
