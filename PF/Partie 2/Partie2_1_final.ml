(* ========================================================================== *)
(* CODE FINAL PARTIE 2 : ANALYSEUR COMPLET (WHILEb avec Priorités & Espaces)  *)
(* ========================================================================== *)
#use "anacomb.ml";;

(* 1. TYPES AST *)
type var = A | B | C | D
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
  | If of expr * prog * prog 
  | While of expr * prog

(* 2. LEXER & BLANCS (Ex 2.1.4) *)
let list_of_string s = let rec b i = if i=String.length s then [] else s.[i]::b(i+1) in b 0

(* Gestion robuste des blancs *)
let spaces = star (terminal_cond (fun c -> c = ' ' || c = '\n' || c = '\t'))
let symbol c = spaces --> terminal c     (* Cherche 'c' après avoir mangé les espaces *)
let token p = spaces -+> p               (* Lance le parseur 'p' après avoir mangé les espaces *)

(* Primitives *)
let parse_var = terminal_res (function 'a'->Some A|'b'->Some B|'c'->Some C|'d'->Some D|_->None)
let parse_digit = terminal_res (function '0'->Some 0|'1'->Some 1|_->None)

(* 3. PARSEUR RECURSIF (Ex 2.1.3 + 2.1.4) *)
(* Hierarchie : Atom -> Factor -> Term -> Expr -> Stmt -> Prog *)

let rec parse_atom l = 
  (
    (token parse_digit ++> fun n -> epsilon_res (Const n)) 
    +| (token parse_var ++> fun v -> epsilon_res (Var v)) 
    +| (symbol '(' -+> parse_expr ++> fun e -> symbol ')' -+> epsilon_res e)
  ) l

and parse_factor l = (* Priorité 1 : ! *)
  ((symbol '!' -+> parse_factor ++> fun e -> epsilon_res (Not e)) +| parse_atom) l

and parse_term l =   (* Priorité 2 : . *)
  (parse_factor ++> fun f -> 
    ((symbol '.' -+> parse_term ++> fun t -> epsilon_res (And(f, t))) +| epsilon_res f)) l

and parse_expr l =   (* Priorité 3 : + *)
  (parse_term ++> fun t -> 
    ((symbol '+' -+> parse_expr ++> fun e -> epsilon_res (Or(t, e))) +| epsilon_res t)) l

and parse_stmt l = 
  ( 
    (* Skip : le mot doit être collé, mais peut avoir des espaces avant *)
    (symbol 's' --> terminal 'k' --> terminal 'i' --> terminal 'p' -+> epsilon_res Skip)
    
    +| (token parse_var ++> fun v -> 
          symbol ':' --> terminal '=' -+> parse_expr ++> fun e -> 
          epsilon_res (Assign(v, e)))
        
    +| (symbol 'i' --> symbol '(' -+> parse_expr ++> fun c -> 
          symbol ')' --> symbol '{' -+> parse_prog ++> fun p1 -> 
          symbol '}' --> symbol '{' -+> parse_prog ++> fun p2 -> 
          symbol '}' -+> epsilon_res (If(c, p1, p2)))
        
    +| (symbol 'w' --> symbol '(' -+> parse_expr ++> fun c -> 
          symbol ')' --> symbol '{' -+> parse_prog ++> fun p -> 
          symbol '}' -+> epsilon_res (While(c, p)))
  ) l

and parse_prog l = 
  (
    (parse_stmt ++> fun s -> 
      (symbol ';' -+> parse_prog ++> fun suite -> epsilon_res (Seq(s, suite)))
      +| epsilon_res s
    )
    +| epsilon_res Skip (* Gère le programme vide / fin de bloc *)
  ) l
;;

(* 4. FONCTION PRINCIPALE *)
let parse s = 
  try 
    let (res, reste) = parse_prog (list_of_string s) in
    let final_reste = spaces reste in (* On consomme les espaces finaux *)
    if final_reste = [] then res else raise Echec
  with Echec -> Skip
;;

(* test final *)

let test_final () =
  Printf.printf "--- Test Final Unifié ---\n";
  (* Expression : !a + b . c  (Doit être : (!a) OR (b AND c)) *)
  (* Formatage  : Espaces partout, retours à la ligne *)
  let code = "
    w ( !a + b . c ) {
       skip ;
       a := 1
    }
  " in
  let ast = parse code in
  match ast with
  | While(Or(Not(Var A), And(Var B, Var C)), Seq(Skip, Assign(A, Const 1))) ->
      Printf.printf "[SUCCES] Tout fonctionne (Grammaire, Priorités, Espaces) !\n"
  | _ -> 
      Printf.printf "[ECHEC] Erreur de structure.\n"
;;

test_final ();;
