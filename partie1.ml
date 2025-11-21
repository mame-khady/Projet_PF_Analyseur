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
  Program := Stmt | Stmt ';' Program
  Stmt := Assign | If | While | Skip
  Assign := Var ':=' Expr
  If := 'i' '(' Var ')' '{' Program '}' '{' Program '}'
  While := 'w' '(' Var ')' '{' Program '}'
  Skip := epsilon
  Expr := '0' | '1' | Var
  Var := 'a' | 'b' | 'c' | 'd'
 *)

(*Exercice 1.1.3*)

(*
  
  Grammaire sans recurssion :
  Program ::= Stmt Program_SE
  Program_SE ::= ';'  Stmt Program_SE | epsilon
  Stmt := Assign | If | While | Skip
  Assign := Var ':=' Expr
  If := 'i' '(' Var ')' '{' Program '}' '{' Program '}'
  While := 'w' '(' Var ')' '{' Program '}'
  Skip := epsilon
  Expr := '0' | '1' | Var
  Var := 'a' | 'b' | 'c' | 'd'
  
 *)





