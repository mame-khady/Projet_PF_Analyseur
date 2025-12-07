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

(*
  Grammaire sans recurssion :
  
  Program ::= Stmt Program_SE
  Program_SE ::= ';'  Stmt Program_SE | epsilon
  Stmt ::= Assign | If | While | Skip
  Assign ::= Var ':=' Expr
  If ::= 'i' '(' Var ')' '{' Program '}' '{' Program '}'
  While ::= 'w' '(' Var ')' '{' Program '}'
  Skip ::= epsilon
  Expr ::= '0' | '1' | Var
  Var ::= 'a' | 'b' | 'c' | 'd'
  
 *)


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



(* TESTS DU PARSEUR *)

(* 1. Tests des variables *)
assert (parse_var (list_of_string "a") = (A, []));
assert (parse_var (list_of_string "b") = (B, []));
assert (parse_var (list_of_string "dXYZ") = (D, ['X';'Y';'Z']));

(* 2. Tests des expressions *)
assert (parse_expr (list_of_string "0") = (Const 0, []));
assert (parse_expr (list_of_string "1") = (Const 1, []));
assert (parse_expr (list_of_string "a") = (Var A, []));
assert (parse_expr (list_of_string "d") = (Var D, []));

(* 3. Assignations *)
assert (
  parse_stmt (list_of_string "a:=0")
  = (Assign (A, Const 0), [])
);

assert (
  parse_stmt (list_of_string "b:=1")
  = (Assign (B, Const 1), [])
);

assert (
  parse_stmt (list_of_string "c:=a")
  = (Assign (C, Var A), [])
);

(* 4. If simple *)
assert (
  parse_stmt (list_of_string "i(a){b:=0}{c:=1}")
  = (If (A, Assign(B,Const 0), Assign(C,Const 1)), [])
);

(* 5. While simple *)
assert (
  parse_stmt (list_of_string "w(a){b:=1}")
  = (While (A, Assign(B,Const 1)), [])
);

(* 6. Séquences *)
assert (
  parse_prog (list_of_string "a:=0;b:=1")
  = (Seq(Assign(A,Const 0), Assign(B,Const 1)), [])
);

assert (
  parse_prog (list_of_string "a:=0;b:=1;c:=1")
  =
 (Seq (Assign (A, Const 0), Seq (Assign (B, Const 1), Assign (C, Const 1))),
 [])
);

(* 7. If avec programme composé *)

assert (
  parse_prog (list_of_string "i(a){a:=0;b:=1}{c:=1}")
  =
  (If (A, Seq (Assign (A, Const 0), Assign (B, Const 1)), Assign (C, Const 1)),
 [])
);

(* 8. While avec programme composé *)
assert (
  parse_prog (list_of_string "w(a){a:=0;b:=1}")
  =
  (While(
     A,
     Seq(Assign(A,Const 0), Assign(B,Const 1))
   ), [])
);

(* 9. Programme complexe *)
assert (
  parse_prog (list_of_string "a:=0;b:=1;i(a){b:=0}{c:=1}")
  =
 (Seq (Assign (A, Const 0),
  Seq (Assign (B, Const 1), If (A, Assign (B, Const 0), Assign (C, Const 1)))),
 [])
);

(* 10. Programme encore plus gros *)

assert (
  parse_prog (list_of_string "a:=0;w(a){b:=1;c:=0};i(b){a:=1}{d:=0}")
  =
 (Seq (Assign (A, Const 0),
  Seq (While (A, Seq (Assign (B, Const 1), Assign (C, Const 0))),
   If (B, Assign (A, Const 1), Assign (D, Const 0)))),
 [])
);

print_endline "Tous les tests du parseur sont VALIDÉS !";;
