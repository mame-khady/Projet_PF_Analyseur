(*exo 1.1.1*)

type var = A | B | C | D;;

type expr =
  Const of int
| Var of var ;;

type progA =
  | Skip
  | Assign of var * expr
  | Seq of progA * progA
  | If of var * progA * progA
  | While of var * progA
;;

let whileb'' =
  Seq (
    Assign(A ,(Const 1)),
    Seq (
        Assign (B,(Const 1)),
        Seq (
            Assign (C,(Const 1)),
  While (A,
         (If (C,
              Seq ((Assign (C, (Const 0))),(Assign (A,( Const 1)))),
              Seq ((Assign (B ,(Const 0))),(Assign (C ,(Const 1))))
         ))
    )
          )
      )
    );;
(*Exercice 1.1.2*)

(*Grammaire
  Program ::= Program ';' Stmt   | Stmt
  Stmt := Assign | If | While | Skip
  Assign ::= Var ':=' Expr
  If ::= 'i' '(' Var ')' '{' Program '}' '{' Program '}'
  While ::= 'w' '(' Var ')' '{' Program '}'
  Skip ::= epsilon
  Expr ::= '0' | '1' | Var
  Var ::= 'a' | 'b' | 'c' | 'd'
 *)

(*Exercice 1.1.3*)

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

(*Exercice 1.1.4*)
(*
  grammaire non recursive :
  C ::= '0' | '1'
  V ::= 'a' | 'b' | 'c' | 'd'
  A ::= C | V
  E ::= T SE 
  SE ::=  '+' T SE | epilon
  T ::= F ST
  ST ::=  '.' F ST | epsilon
  F::= '!' F | A | '(' E ')'
 *)

(*exercice 1.2.1*)

(*
  Pour un programme de la forme if expr then P else Q , on a deux régles selon que l'évaluation de expr dans l'état s1 donne false ou true.

                          P                                                        Q
  [[expr]]s1 = true  s1 ====> s2                       [[expr]]s1 = false   s1 =======> s2
  ------------------------------------               ----------------------------------------------------
          if expr then P else Q                                  if expr then P else Q
     s1==========================>s2                       s1============================>s2
*)



