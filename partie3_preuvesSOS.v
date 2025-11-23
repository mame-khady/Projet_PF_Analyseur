Require Import Bool Arith List.
Import List.ListNotations.
Inductive aexp :=
| Aco : nat -> aexp
| Ava : nat -> aexp
| Apl : aexp -> aexp -> aexp
| Amu : aexp -> aexp -> aexp
| Amo : aexp -> aexp -> aexp
.
Inductive bexp :=
| Btrue : bexp
| Bfalse : bexp
| Bnot : bexp -> bexp
| Band : bexp -> bexp -> bexp
| Bor : bexp -> bexp -> bexp
| Beq : bexp -> bexp -> bexp
| Beqnat : aexp -> aexp -> bexp
.
Inductive winstr :=
| Skip : winstr
| Assign : nat -> aexp -> winstr
| Seq : winstr ->winstr -> winstr
| If : bexp -> winstr -> winstr -> winstr
| While : bexp -> winstr -> winstr
.
Definition state := list nat.

Fixpoint get (x:nat) (s:state) : nat :=
match x,s with
| 0 , v::_ => v
| S x1, _::l1  => get x1 l1
| _ , _  => 0
end.

Fixpoint update (s:state) (v:nat) (n:nat): state :=
match v,s with
| 0 , a :: l1  => n :: l1
| 0 , nil  => n :: nil
| S v1, a :: l1  => a :: (update l1 v1 n)
| S v1, nil  => 0 :: (update nil v1 n)
end.
Fixpoint evalA (a: aexp) (s: state) : nat :=
match a with
| Aco n  => n
| Ava x  => get x s
| Apl a1 a2  => evalA a1 s + evalA a2 s
| Amu a1 a2  =>evalA a1 s * evalA a2 s
| Amo a1 a2  => evalA a1 s - evalA a2 s
end.

Definition eqboolb b1 b2 : bool :=
match b1, b2 with
| true , true  => true
| false, false  => true
| _ , _  => false
end.

Fixpoint eqnatb n1 n2 : bool :=
match n1, n2 with
| O , O  => true
| S n1', S n2'  => eqnatb n1' n2'
| _ , _  => false
end.

Fixpoint evalB (b : bexp) (s : state) : bool :=
match b with
| Btrue  => true
| Bfalse  => false
| Bnot b  => negb (evalB b s)
| Band e1 e2  => (evalB e1 s) && (evalB e2 s)
| Bor e1 e2  => (evalB e1 s) || (evalB e2 s)
| Beq e1 e2  => eqboolb (evalB e1 s) (evalB e2 s)
| Beqnat n1 n2  => eqnatb (evalA n1 s) (evalA n2 s)
end.
Inductive config :=
| Inter : winstr -> state -> config
| Final : state -> config.


(* La relation pour un pas de SOS *)

Inductive SOS_1: winstr -> state -> config -> Prop :=
| SOS1_Skip     : forall s,
SOS_1 Skip s (Final s)

| SOS1_Assign   : forall x a s,
SOS_1 (Assign x a) s (Final (update s x (evalA a s)))

| SOS1_Seqf     : forall i1 i2 s s1,
SOS_1 i1 s (Final s1) ->
SOS_1 (Seq i1 i2) s (Inter i2 s1)
| SOS1_Seqi     : forall i1 i1' i2 s s1,
                 SOS_1 i1 s (Inter i1' s1) ->
SOS_1 (Seq i1 i2) s (Inter (Seq i1' i2) s1)

| SOS1_If_true  : forall b i1 i2 s,
                 evalB b s = true  ->
                 SOS_1 (If b i1 i2) s (Inter i1 s)
| SOS1_If_false : forall b i1 i2 s,
                 evalB b s = false ->
                 SOS_1 (If b i1 i2) s (Inter i2 s)

| SOS1_While    : forall b i s,
                 SOS_1 (While b i) s (Inter (If b (Seq i (While b i)) Skip) s)
.

Inductive SOS : config -> config -> Prop :=
| SOS_stop : forall c, SOS c c
| SOS_again : forall i1 s1 c2 c3,
              SOS_1 i1 s1 c2 -> SOS c2 c3 ->
              SOS (Inter i1 s1) c3.
(*Exercice 3.1.1 - Propriétés générales*)
(** La relation SOS est transitive *)
Theorem SOS_trans : forall c1 c2 c3, SOS c1 c2 -> SOS c2 c3 -> SOS c1 c3.
Proof.
  intros c1 c2 c3 H12 H23.
  induction H12 as [c | i1 s1 c2' c3' H1 H23' IH].
- (* Cas SOS_stop : c1 = c2 *)
assumption.
- (* Cas SOS_again *)
eapply SOS_again.
+ exact H1.
+ apply IH. exact H23.
Qed.
(*Énoncé de SOS_seq en français : Si on peut passer d'une configuration intermédiaire avec l'instruction i1 et l'état s1 à une configuration finale avec l'état s2, alors on peut passer d'une configuration avec la séquence Seq i1 i2 et l'état s1 à une configuration intermédiaire avec l'instruction i2 et l'état s2.*)
(*Exercice 3.1.2 - Programme Pcarre_2*)
(** Premier tour de boucle : de [0;0;1] à [1;1;3] *)

(* ========================================================================== *)

Definition N0 := Aco 0.
Definition N1 := Aco 1.
Definition N2 := Aco 2.
Definition N3 := Aco 3.
Definition N4 := Aco 4.


(** * I *)
(** *** Calcul du carré avec des additions *)
(** On code dans While un programme Pcarre correspondant à
while not (i=n) do {i:= 1+i; x:= y+x ; y:= 2+y} *)
Definition Il := 0.
Definition Ir := Ava Il.
Definition Xl := 1.
Definition Xr := Ava Xl.
Definition Yl := 2.
Definition Yr := Ava Yl.

Definition incrI := Assign Il (Apl N1 Ir).
Definition incrX := Assign Xl (Apl Yr Xr).
Definition incrY := Assign Yl (Apl N2 Yr).
Definition corps_carre := Seq incrI (Seq incrX incrY).
Definition Pcarre_2 := While (Bnot (Beqnat Ir (Aco 2))) corps_carre.
Definition Pcarre n := While (Bnot (Beqnat Ir (Aco n))) corps_carre.
(** Nouveau : on peut jouer avec des programmes qui bouclent *)
Definition Pcarre_inf := While Btrue corps_carre.
Lemma SOS_Pcarre_2_1er_tour :
SOS (Inter Pcarre_2 [0;0;1]) (Inter Pcarre_2 [1; 1; 3]).
Proof.
unfold Pcarre_2.
(* Déroulement de While *)
eapply SOS_again.
{ apply SOS1_While. }
(* Évaluation de la condition (0 <> 2 = true) *)
eapply SOS_again.
{ apply SOS1_If_true. cbn. reflexivity. }
(* Exécution du corps : incrI *)
unfold corps_carre.
eapply SOS_again.
{ eapply SOS1_Seqi. eapply SOS1_Seqf.
unfold incrI. apply SOS1_Assign. }
cbn.
(* Exécution de incrX *)
eapply SOS_again.
{ eapply SOS1_Seqi. eapply SOS1_Seqf.
unfold incrX. apply SOS1_Assign. }
cbn.
(* Exécution de incrY *)
eapply SOS_again.
{ eapply SOS1_Seqf. unfold incrY. apply SOS1_Assign. }
cbn.
apply SOS_stop.
Qed.
(** Pcarre_inf fait un tour de boucle *)
Theorem SOS_Pcarre_inf_1er_tour :
SOS (Inter Pcarre_inf [0;0;1]) (Inter Pcarre_inf [1; 1; 3]).
Proof.
unfold Pcarre_inf.
(* While Btrue se déroule toujours *)
eapply SOS_again.
{ apply SOS1_While. }
(* La condition est toujours vraie *)
eapply SOS_again.
{ apply SOS1_If_true. cbn. reflexivity. }
(* Corps de boucle identique à Pcarre_2 *)
unfold corps_carre.
eapply SOS_again.
{ eapply SOS1_Seqi. eapply SOS1_Seqf.
unfold incrI. apply SOS1_Assign. }
cbn.
eapply SOS_again.
{ eapply SOS1_Seqi. eapply SOS1_Seqf.
unfold incrX. apply SOS1_Assign. }
cbn.
eapply SOS_again.
{ eapply SOS1_Seqf. unfold incrY. apply SOS1_Assign. }
cbn.
apply SOS_stop.
Qed.

(*Signification de SOS_Pcarre_inf_1er_tour : Le programme qui boucle infiniment fait un tour de boucle en partant de l'état initial [0;0;1] et arrive à l'état [1;1;3].*)

(*Le théorème SOS_Pcarre_2_2e_tour exprime qu'a partir d'une configuration intermédiaire où le programme Pcarre_2 doit être exécuté avec l'état [1; 1; 3] (c'est-à-dire i=1, x=1, y=3), la sémantique opérationnelle à petits pas (SOS) permet d'atteindre une nouvelle configuration intermédiaire où Pcarre_2 doit être exécuté avec l'état [2; 4; 5] (c'est-à-dire i=2, x=4, y=5).*)
(** Deuxième tour de boucle *)
Lemma SOS_Pcarre_2_2e_tour :
  SOS (Inter Pcarre_2 [1; 1; 3]) (Inter Pcarre_2 [2; 4; 5]).
Proof.
  unfold Pcarre_2.
  eapply SOS_again.
  { apply SOS1_While. }
  eapply SOS_again.
  { apply SOS1_If_true. cbn. reflexivity. }
  unfold corps_carre.
  eapply SOS_again.
  { eapply SOS1_Seqi. eapply SOS1_Seqf.
    unfold incrI. apply SOS1_Assign. }
  cbn.
  eapply SOS_again.
  { eapply SOS1_Seqi. eapply SOS1_Seqf.
    unfold incrX. apply SOS1_Assign. }
  cbn.
  eapply SOS_again.
  { eapply SOS1_Seqf. unfold incrY. apply SOS1_Assign. }
  cbn.
  apply SOS_stop.
Qed.
(*Signification de SOS_Pcarre_2_fini : Quand i atteint 2, la boucle s'arrête car la condition (i ≠ 2) devient fausse, et le programme termine.*)
Theorem SOS_Pcarre_2_fini :
SOS (Inter Pcarre_2 [2; 4; 5]) (Final [2; 4; 5]).
Proof.
unfold Pcarre_2.
eapply SOS_again.
{ apply SOS1_While. }
eapply SOS_again.
{ apply SOS1_If_false. cbn. reflexivity. }
eapply SOS_again.
{ apply SOS1_Skip. }
apply SOS_stop.
Qed.
(** Version complète avec transitivité *)
Theorem SOS_Pcarre_2_V0 :
SOS (Inter Pcarre_2 [0;0;1]) (Final [2;4;5]).
Proof.
apply SOS_trans with (Inter Pcarre_2 [1; 1; 3]).
{ apply SOS_Pcarre_2_1er_tour. }
apply SOS_trans with (Inter Pcarre_2 [2; 4; 5]).
{ apply SOS_Pcarre_2_2e_tour. }
apply SOS_Pcarre_2_fini.
Qed.
(*Exercice 3.1.3 - Programmes généraux*)
(** Lemme arithmétique pour eqnatb *)
Lemma eqnatb_refl : forall n, eqnatb n n = true.
Proof.
induction n as [| n' IH].
  - reflexivity.
  - simpl. exact IH.
Qed.
(*Signification de SOS_corps_carre : Le corps de la boucle transforme l'invariant (n, n², 2n+1) en (n+1, (n+1)², 2(n+1)+1).*)
Lemma Sn_2 n : S n + S n = S (S (n + n)).
Proof. ring. Qed.

Lemma Sn_carre n : S n * S n = S (n + n + n * n).
Proof. ring. Qed.

Definition invar_cc n := [n; n*n; S (n+n)].
Lemma SOS_seqf : forall i1 i2 s1 s2,
SOS (Inter i1 s1) (Final s2) ->
SOS (Inter (Seq i1 i2) s1) (Inter i2 s2).
Proof.
intros i1 i2 s1 s2 Hso.
remember (Inter i1 s1) as c1 eqn:Heqc1.
remember (Final s2) as cf eqn:Heqcf.
revert i1 s1 Heqc1.
induction Hso; intros.

- (* Cas SOS_stop: c = c *)
rewrite Heqc1 in Heqcf.
inversion Heqcf.

- (* Cas SOS_again *)
injection Heqc1 as <- <-.
destruct c2 as [i' s' | s'].

    + (* c2 = Inter i' s' *)
      eapply SOS_again.
      * apply SOS1_Seqi. exact H.
      * apply IHHso.
        -- exact Heqcf.
        -- reflexivity.

    + (* c2 = Final s' *)
eapply SOS_again.
* apply SOS1_Seqf. exact H.
* (* SOS (Final s') c3 et c3 = Final s2 *)
        (* Donc s' = s2 *)
assert (s' = s2) as Heqs.
        { subst c3.  (* Maintenant Hso : SOS (Final s') (Final s2) *)
inversion Hso; subst.
- reflexivity.}
subst.
apply SOS_stop.
Qed.
Theorem SOS_corps_carre n :
SOS (Inter corps_carre (invar_cc n)) (Final (invar_cc (S n))).
Proof.
unfold corps_carre, invar_cc.

eapply SOS_again.
{ apply SOS1_Seqf. unfold incrI. apply SOS1_Assign. }
cbn.

eapply SOS_again.
{ apply SOS1_Seqf. unfold incrX. apply SOS1_Assign. }
cbn.

eapply SOS_again.
{ unfold incrY. apply SOS1_Assign. }
cbn.

(* Prouver l'égalité des états *)
  assert (Heq : [S n; S (n + n + n * n); S (S (S (n + n)))] =
                [S n; S n * S n; S (S n + S n)]).
  {
    repeat f_equal.
    - (* S (n + n + n * n) = S n * S n *)
      symmetry. apply Sn_carre.
    - (* S (S (S (n + n))) = S (S n + S n) *)
      rewrite Sn_2.
      reflexivity.
  }

  rewrite Heq.
  apply SOS_stop.
Qed.
(*Signification de SOS_corps_carre_inter : En séquençant le corps avec une instruction i, on atteint la configuration où il reste i à exécuter.*)
Lemma SOS_corps_carre_inter n i :
  SOS (Inter (Seq corps_carre i) (invar_cc n)) (Inter i (invar_cc (S n))).
Proof.
  apply SOS_seqf.
  apply SOS_corps_carre.
Qed.
(*Signification de SOS_Pcarre_tour : Un tour de la boucle Pcarre fait passer de l'invariant i à l'invariant (i+1), tant que i ≠ n.*)
Lemma SOS_Pcarre_tour :
  forall n i, eqnatb i n = false ->
  SOS (Inter (Pcarre n) (invar_cc i)) (Inter (Pcarre n) (invar_cc (S i))).
Proof.
  intros n i Hneq.
  unfold Pcarre.
  eapply SOS_again.
  { apply SOS1_While. }
  eapply SOS_again.
  { apply SOS1_If_true. cbn. rewrite Hneq. reflexivity. }
  apply SOS_corps_carre_inter.
Qed.
(*Signification de SOS_Pcarre_n_fini : Quand i = n, la boucle termine.*)
Theorem SOS_Pcarre_n_fini :
  forall n, SOS (Inter (Pcarre n) (invar_cc n)) (Final (invar_cc n)).
Proof.
  intros n.
  unfold Pcarre.
  eapply SOS_again.
  { apply SOS1_While. }
  eapply SOS_again.
  { apply SOS1_If_false. cbn. rewrite eqnatb_refl. reflexivity. }
  eapply SOS_again.
  { apply SOS1_Skip. }
  apply SOS_stop.
Qed.
(*Explication de SOS_Pcarre_2_fin_V2 : On fait deux tours de boucle (i=0→1, puis i=1→2) puis on termine. Chaque étape utilise la transitivité.*)
Lemma SOS_Pcarre_inf_tour :
  forall i,
  SOS (Inter Pcarre_inf (invar_cc i)) (Inter Pcarre_inf (invar_cc (S i))).
Proof.
  intros i.
  unfold Pcarre_inf.
  eapply SOS_again.
  { apply SOS1_While. }
  eapply SOS_again.
  { apply SOS1_If_true. reflexivity. }
  apply SOS_corps_carre_inter.
Qed.
(*Signification : Pcarre_inf fait toujours un tour supplémentaire.*)
Theorem SOS_Pcarre_inf_i :
  forall i,
  SOS (Inter Pcarre_inf [0; 0; 1]) (Inter Pcarre_inf (invar_cc i)).
Proof.
  induction i as [| i' IH].
- unfold invar_cc. cbn. apply SOS_stop.
- apply SOS_trans with (Inter Pcarre_inf (invar_cc i')).
    + exact IH.
    + apply SOS_Pcarre_inf_tour.
Qed.
(*Exercice 3.1.4 - Version fonctionnelle*)
Fixpoint f_SOS_1 (i : winstr) (s : state) : config :=
  match i with
  | Skip => Final s
  | Assign x a => Final (update s x (evalA a s))
  | Seq i1 i2 =>
      match f_SOS_1 i1 s with
      | Final s1 => Inter i2 s1
      | Inter i1' s1 => Inter (Seq i1' i2) s1
      end
  | If b i1 i2 =>
      if evalB b s then Inter i1 s else Inter i2 s
  | While b i =>
      Inter (If b (Seq i (While b i)) Skip) s
  end.
Lemma f_SOS_1_corr : forall i s, SOS_1 i s (f_SOS_1 i s).
Proof.
  induction i; intros s; simpl.
  - apply SOS1_Skip.
  - apply SOS1_Assign.
  - destruct (f_SOS_1 i1 s) eqn:E.
    + (* f_SOS_1 i1 s = Inter w s0 *)
      apply SOS1_Seqi.
      rewrite <- E.  (* Remplace Inter w s0 par f_SOS_1 i1 s *)
      apply IHi1.
    + (* f_SOS_1 i1 s = Final s0 *)
      apply SOS1_Seqf.
      rewrite <- E.  (* Remplace Final s0 par f_SOS_1 i1 s *)
      apply IHi1.
  - destruct (evalB b s) eqn:E.
    + apply SOS1_If_true. exact E.
    + apply SOS1_If_false. exact E.
  - apply SOS1_While.
Qed.
Lemma f_SOS_1_compl : forall i s c, SOS_1 i s c -> c = f_SOS_1 i s.
Proof.
  intros i s c H.
  induction H; simpl.

  - (* SOS1_Skip *)
    reflexivity.

  - (* SOS1_Assign *)
    reflexivity.

  - (* SOS1_Seqf : SOS_1 i1 s (Final s1) -> SOS_1 (Seq i1 i2) s (Inter i2 s1) *)
    rewrite <- IHSOS_1.
    destruct (f_SOS_1 i1 s); reflexivity.

  - (* SOS1_Seqi : SOS_1 i1 s (Inter i1' s1) -> SOS_1 (Seq i1 i2) s (Inter (Seq i1' i2) s1) *)
    rewrite <- IHSOS_1.
    destruct (f_SOS_1 i1 s); reflexivity.

  - (* SOS1_If_true *)
    rewrite H. reflexivity.

  - (* SOS1_If_false *)
    rewrite H. reflexivity.

  - (* SOS1_While *)
    reflexivity.
Qed.
