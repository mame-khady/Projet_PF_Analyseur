(* TD - Compilation certifiée des expressions arithmétiques  *)
(* Pierre Corbineau, Jean-Francois Monin                     *)
(*            UGA Polytech RICM4  2021                       *)

Require Import Nat List.
Import List.ListNotations.

(* ============ Définitions préliminaires ============ *)

(** * Syntaxe des expressions arithétiques *)

Inductive aexp :=
| Aco : nat -> aexp (** constantes *)
| Ava : nat -> aexp (** variables *)
| Apl : aexp -> aexp -> aexp
| Amu : aexp -> aexp -> aexp
| Amo : aexp -> aexp -> aexp
.

Definition state := list nat.

Fixpoint get (x:nat) (s:state) : nat :=
match x,s with
| 0,v::_      => v
| S x1, _::l1 => get x1 l1
| _,_         => 0
end.

(** * Sémantique fonctionnelle pour AExp *)
Fixpoint evalA (a: aexp) (s: state) : nat :=
match a with
| Aco n => n
| Ava x => get x s
| Apl a1 a2 =>  evalA a1 s + evalA a2 s
| Amu a1 a2 =>  evalA a1 s * evalA a2 s
| Amo a1 a2 =>  evalA a1 s - evalA a2 s
end.

(** * Un état pour faire des tests *)

Definition S2 := [0; 3].

(** * Un exemple d'expression pour faire des tests *)

Definition X := Ava 1.

(* ================   Compilation de aexp   ======================= *)

(* Langage d'assemblage à pile (als) *)

Inductive instr_als : Set :=
| Ipush : nat -> instr_als
| Ifetch : nat -> instr_als
| Iadd :  instr_als
| Isub :  instr_als
| Imul :  instr_als.

(* Un code est une liste d'instruction *)

Definition CODE := list instr_als.

(* Sémantique fonctionnelle (dénotationnelle) du langage  *)

Definition stack := list nat.

Definition push n : stack -> stack := fun st => n :: st.

(* L'exécution générique d'un opérateur binaire *)

Definition exec_opbin (op : nat -> nat -> nat) : stack -> stack :=
  fun s => match s with
           | b :: a :: s => op a b :: s
           | _ => nil
           end.

(* Sémantique fonctionnelle d'une instruction *)

Definition exec_i (i : instr_als) (s:state) : stack -> stack :=
match i with
| Ipush n => push n
| Ifetch x => push (get x s)
| Iadd => exec_opbin Nat.add
| Isub => exec_opbin Nat.sub
| Imul => exec_opbin Nat.mul
end.

(* Sémantique fonctionnelle d'un code *)

Fixpoint exec (c : CODE) (s:state) (p:stack): stack :=
    match c with
      | nil => p
      | i :: c' =>
let p' := (exec_i i s p) in
        exec c' s p'
    end.

(* Lemme de décomposition *)
Lemma dec_code :  forall c1 c2 s p, exec (c1++c2) s p = exec c2 s (exec c1 s p).
Proof.
  intros c1 c2 s.
  induction c1 as [| i1 c1 Hrec_c1]; simpl; intro p.
  - (* c1 = [] *)
    reflexivity.
  - (* c1 = i1 :: c1 *)
    rewrite Hrec_c1.
    reflexivity.
Qed.
(* Lemme de composition *)
Lemma comp_exec : forall c1 c2 s p1 p2 p3,
                  exec c1 s p1 = p2
               -> exec c2 s p2 = p3
               -> exec (c1++c2) s p1 = p3.
Proof.
  intros c1 c2 s p1 p2 p3 H1 H2.
  rewrite dec_code.
  rewrite H1.
  exact H2.
Qed.


(* Compilateur *)

Fixpoint compileA (a : aexp) : CODE :=
  match a with
    | Aco n      => [Ipush n]
    | Ava x      => [Ifetch x]
    | Apl a1 a2  => compileA a1 ++ compileA a2 ++ [Iadd]
    | Amu a1 a2  => compileA a1 ++ compileA a2 ++ [Imul]
    | Amo a1 a2  => compileA a1 ++ compileA a2 ++ [Isub]
  end.

(* Exemples *)

Definition ae2px := Apl (Aco 2) X. (* 2 + x *)
Definition ae9m2px := Amo (Aco 9) ae2px. (* 9 - (2 + x) *)

Compute (compileA ae2px).
Compute ae9m2px.
Compute (compileA ae9m2px).

Compute exec (compileA (Aco 2)) S2 nil.
Compute exec (compileA ae2px) S2 nil.
Compute exec (compileA ae9m2px) S2 nil.

(* ============ Théorème de correction ============ *)

Theorem correct_compileA_allp :
  forall (a : aexp) (s : state) (p:stack),
    exec (compileA a) s p = exec [Ipush (evalA a s)] s p.
Proof.
  induction a as [n | x | a1 Ha1 a2 Ha2 | a1 Ha1 a2 Ha2 | a1 Ha1 a2 Ha2];
    intros s p; cbn.
  - (* Aco n *)
    reflexivity.
  - (* Ava x *)
    reflexivity.
  - (* Apl a1 a2 *)
    rewrite dec_code.
    rewrite dec_code.
    rewrite Ha1.
    cbn.
    rewrite Ha2.
    cbn.
    reflexivity.
  - (* Amu a1 a2 *)
    rewrite dec_code.
    rewrite dec_code.
    rewrite Ha1.
    cbn.
    rewrite Ha2.
    cbn.
    reflexivity.
  - (* Amo a1 a2 *)
    rewrite dec_code.
    rewrite dec_code.
    rewrite Ha1.
    cbn.
    rewrite Ha2.
    cbn.
    reflexivity.
Qed.

Corollary correct_compileA : forall (a : aexp) (s:state),
                             exec (compileA a) s [] = [evalA a s].
Proof.
  intros a s.
  apply correct_compileA_allp.
Qed.

(* ============ Partie optionnelle ============ *)

(* Contre-exemple pour app_stack général *)

Fact wrong_app_stack : exists c s p1 p2, exec c s (p1 ++ p2) <> (exec c s p1) ++ p2.
Proof.
  exists [Iadd].  (* code qui fait une addition *)
  exists S2.      (* état quelconque *)
  exists [].      (* pile vide *)
  exists [1; 2].  (* pile avec deux éléments *)
  simpl.
  intro H.
  discriminate.
Qed.

(* Propriété triviale *)
Remark app_comm_cons {A} : forall (x y:list A) (a:A), a :: (x ++ y) = (a :: x) ++ y.
Proof. reflexivity. Qed.
(* app_stack pour le code compilé *)
Lemma app_stack : forall a s p1 p2,
    exec (compileA a) s (p1++p2) = (exec (compileA a) s p1)++p2.
Proof.
  (* à compléter *)
Admitted.

(** Extraction *)
Require Import Extraction.
Extraction compileA.
Recursive Extraction compileA.