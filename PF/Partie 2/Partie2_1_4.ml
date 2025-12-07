(* ========================================================================== *)
(* EXERCICE 2.1.4 : Robustesse aux blancs                                     *)
(* Objectif : Prouver que " a := 1 " fonctionne comme "a:=1"                  *)
(* ========================================================================== *)
#use "anacomb.ml";;

(* TYPES *)
type var = A | B | C | D
type expr = Const of int | Var of var | Not of expr | And of expr * expr | Or of expr * expr
type prog = Skip | Assign of var * expr | Seq of prog * prog | If of expr * prog * prog | While of expr * prog

(* LEXER & UTILITAIRES *)
let list_of_string s = let rec b i = if i=String.length s then [] else s.[i]::b(i+1) in b 0

(* 1. Defini les espaces (espace, tab, retour ligne) *)
let spaces = star (terminal_cond (fun c -> c = ' ' || c = '\n' || c = '\t'))

(* 2. 'symbol' mange les espaces AVANT de chercher le caractère c *)
let symbol c = spaces --> terminal c

(* 3. 'token' mange les espaces AVANT de lancer un parseur p *)
let token p = spaces -+> p

(* Parseurs de bases *)
let parse_var = terminal_res (function 'a'->Some A|'b'->Some B|'c'->Some C|'d'->Some D|_->None)
let parse_digit = terminal_res (function '0'->Some 0|'1'->Some 1|_->None)

(* PARSEUR EXPRESSIONS (Priorités conservées) *)
let rec parse_atom l = 
  ((token parse_digit ++> fun n -> epsilon_res (Const n)) 
  +| (token parse_var ++> fun v -> epsilon_res (Var v)) 
  +| (symbol '(' -+> parse_expr ++> fun e -> symbol ')' -+> epsilon_res e)) l

and parse_factor l = 
  ((symbol '!' -+> parse_factor ++> fun e -> epsilon_res (Not e)) 
  +| parse_atom) l

and parse_term l = 
  (parse_factor ++> fun f -> 
    (symbol '.' -+> parse_term ++> fun t -> epsilon_res (And(f, t))) 
    +| epsilon_res f) l

and parse_expr l = 
  (parse_term ++> fun t -> 
    (symbol '+' -+> parse_expr ++> fun e -> epsilon_res (Or(t, e))) 
    +| epsilon_res t) l

(* PARSEUR INSTRUCTIONS *)
let rec parse_stmt l = 
  ( 
    (* Espaces avant le 's', et 'kip' doivent être collés *)
    (symbol 's' --> terminal 'k' --> terminal 'i' --> terminal 'p' -+> epsilon_res Skip)
    
  +| (token parse_var ++> fun v -> 
        symbol ':' --> terminal '=' -+> parse_expr ++> fun e -> (* ':' et '=' souvent collés ou séparés, symbol ':' gère l'espace avant *)
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
    (* Gestion du vide / Fin de bloc *)
    +| epsilon_res Skip 
  ) l
;;

(* WRAPPER DE TEST CORRIGÉ *)
let parse s = 
  try 
    let (res, reste) = parse_prog (list_of_string s) in
    
    (* CORRECTION ICI : spaces renvoie juste la liste, pas de tuple *)
    let final_reste = spaces reste in 
    
    if final_reste = [] then res else raise Echec
  with Echec -> 
    Printf.printf "Erreur de syntaxe (ou reste non vide)\n"; 
    Skip
;;

(* TEST BLANCS *)
let run_test_espacesblancs () =
  Printf.printf "\n--- Validation Ex 2.1.4 (Robustesse Blancs) ---\n";

  (* Test 1 : Espaces simples *)
  let t1 = parse " a := 1 " in
  (match t1 with Assign(A, Const 1) -> Printf.printf "[OK] Espaces simples.\n" | _ -> Printf.printf "[FAIL] T1\n");

  (* Test 2 : Retours à la ligne et indentation bizarre *)
  let code_moche = "
    a := 1 ; 
    w ( a ) {
        skip
    }
  " in
  let t2 = parse code_moche in
  (match t2 with Seq(Assign _, While(_, Skip)) -> Printf.printf "[OK] Retours lignes/Tabs.\n" | _ -> Printf.printf "[FAIL] T2\n");

  (* Test 3 : Programme compact sans aucun espace (Doit toujours marcher) *)
  let t3 = parse "a:=1;b:=0" in
  (match t3 with Seq _ -> Printf.printf "[OK] Compact sans espaces.\n" | _ -> Printf.printf "[FAIL] T3\n");
  
  (* Test 4 : Bloc vide avec espaces dedans *)
  let t4 = parse "w(a) {   }" in
  (match t4 with While(_, Skip) -> Printf.printf "[OK] Bloc vide avec espaces.\n" | _ -> Printf.printf "[FAIL] T4\n");
;;

run_test_espacesblancs ();;
