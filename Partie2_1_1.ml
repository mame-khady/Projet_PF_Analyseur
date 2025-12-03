(* ========================================================================== *)
(* EXERCICE 2.1.1 & 2.1.2                                                     *)
(* ========================================================================== *)
#use "anacomb.ml";;

(* 1. DÉFINITION DE L'AST (Types) *)
type var = A | B | C | D
type expr = Const of int | Var of var 

(* Dans WHILEb--, la condition est une variable. *)
type prog = 
  | Skip
  | Assign of var * expr
  | Seq of prog * prog
  | If of var * prog * prog  
  | While of var * prog       

(* 2. ANALYSEURS LEXICAUX ÉLÉMENTAIRES *)

(* Transforme string en liste de char *)
let list_of_string s = 
  let n = String.length s in
  let rec boucle i = if i = n then [] else s.[i] :: boucle (i+1) in 
  boucle 0

(* Parseur de variable : a, b, c, d *)
let parse_var = terminal_res (function
    | 'a' -> Some A
    | 'b' -> Some B
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)

(* Parseur de constante : 0, 1 *)
let parse_digit = terminal_res (function
    | '0' -> Some 0
    | '1' -> Some 1
    | _ -> None)

(* Parseur d'expression : 0, 1 ou Variable *)
let parse_expr =
  (parse_digit ++> fun n -> epsilon_res (Const n))
  +|
  (parse_var ++> fun v -> epsilon_res (Var v))

(* 3. ANALYSEURS SYNTAXIQUES (Mutuellement récursifs avec 'and') *)
let rec parse_stmt l =
  (
    (* Skip *)
    (terminal 's' --> terminal 'k' --> terminal 'i' --> terminal 'p' -+> epsilon_res Skip)
    +|
    (* Assign *)
    (parse_var ++> fun v ->
      terminal ':' --> terminal '=' -+> parse_expr ++> fun e ->
        epsilon_res (Assign(v, e)))
    +|
    (* If *)
    (terminal 'i' --> terminal '(' -+> parse_var ++> fun c ->
      terminal ')' --> terminal '{' -+> parse_prog ++> fun p1 ->
        terminal '}' --> terminal '{' -+> parse_prog ++> fun p2 ->
          terminal '}' -+> epsilon_res (If(c, p1, p2)))
    +|
    (* While *)
    (terminal 'w' --> terminal '(' -+> parse_var ++> fun c ->
      terminal ')' --> terminal '{' -+> parse_prog ++> fun p ->
        terminal '}' -+> epsilon_res (While(c, p)))
  ) l

and parse_prog l =
  (
    (* Instruction suivie éventuellement d'un point-virgule *)
    parse_stmt ++> fun s -> 
      (
        (terminal ';' -+> parse_prog ++> fun suite -> epsilon_res (Seq(s, suite)))
        +| 
        (epsilon_res s)
      )
    (* Ou rien du tout (Skip implicite pour gérer les blocs vides) *)
    +| epsilon_res Skip
  ) l
;;

(* TESTS (Exercice 2.1.2) *)
let test_parser () =
  Printf.printf "\n--- Tests Analyseur WHILEb-- ---\n";

  (* Test 1 : Affectation simple *)
  let p1 = parse "a:=1" in
  Printf.printf "Test 1 (a:=1) : %s\n" 
    (match p1 with Assign(A, Const 1) -> "OK" | _ -> "FAIL");

  (* Test 2 : Séquence *)
  let p2 = parse "a:=1;b:=0" in
  Printf.printf "Test 2 (Seq)  : %s\n" 
    (match p2 with Seq(Assign _, Assign _) -> "OK" | _ -> "FAIL");

  (* Test 3 : Programme Vide (Skip implicite) *)
  let p3 = parse "" in
  Printf.printf "Test 3 (Vide) : %s\n" 
    (match p3 with Skip -> "OK" | _ -> "FAIL");

  (* Test 4 : Le programme complet de l'exercice 1.1 *)
  (* a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}} *)
  let code_complexe = "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}" in
  let p4 = parse code_complexe in
  Printf.printf "Test 4 (Complex): Structure generee !\n";
  
  (* Affichage basique pour vérifier p4 *)
  let rec print_ast = function
    | Skip -> Printf.printf "Skip"
    | Assign _ -> Printf.printf "Assign"
    | Seq(p1, p2) -> Printf.printf "Seq("; print_ast p1; Printf.printf ","; print_ast p2; Printf.printf ")"
    | If _ -> Printf.printf "If"
    | While _ -> Printf.printf "While"
  in
  print_ast p4; Printf.printf "\n"
;;

(* Lancer les tests *)
test_parser ();;
