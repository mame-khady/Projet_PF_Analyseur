(* =============================================================== *)
(* PROJET WHILEb-- : Analyse Syntaxique et Sémantique Naturelle    *)
(* Fichier unique combinant les exercices 2.1, 2.2.1 et 2.2.2      *)
(* =============================================================== *)

(* --------------------------------------------------------------- *)
(* PARTIE 1 : DÉFINITION DE L'AST (ARBRE SYNTAXIQUE ABSTRAIT)      *)
(* --------------------------------------------------------------- *)

type var = A | B | C | D

(* Expressions (Booléennes et Arithmétiques mélangées pour WHILEb) *)
type expr =
  | Const of int
  | Var of var
  | Not of expr
  | And of expr * expr  (* Opérateur '.' *)
  | Or of expr * expr   (* Opérateur '+' *)

(* Instructions du programme *)
(* MODIFICATION EX 2.2.2 : Les conditions sont maintenant des expressions *)
type prog =
  | Skip
  | Assign of var * expr
  | Seq of prog * prog
  | If of expr * prog * prog    (* Avant: var, Maintenant: expr *)
  | While of expr * prog        (* Avant: var, Maintenant: expr *)

(* --------------------------------------------------------------- *)
(* PARTIE 2 : COMBINATEURS D'ANALYSEURS                            *)
(* --------------------------------------------------------------- *)

(* Un analyseur prend une liste de char et renvoie un résultat + le reste *)
type ('res, 'term) ranalist = 'term list -> 'res * 'term list
exception Echec

(* Convertit string en char list *)
let list_of_string s =
  let n = String.length s in
  let rec boucle i = if i = n then [] else s.[i] :: boucle (i+1) in boucle 0

(* Analyseur qui réussit toujours sans manger de caractère *)
let epsilon_res v : ('res, 'term) ranalist = fun l -> (v, l)

(* Séquence (Bind) : p1 puis p2 qui utilise le résultat de p1 *)
let (++>) (p1 : ('a, 't) ranalist) (f : 'a -> ('b, 't) ranalist) : ('b, 't) ranalist =
  fun l -> let (x, l') = p1 l in f x l'

(* Choix (Or) : essaie p1, si erreur essaie p2 *)
let (+|) p1 p2 = fun l -> try p1 l with Echec -> p2 l

(* Séquence simple droite : p1 puis p2, garde p2 *)
let (-->) p1 p2 = p1 ++> fun _ -> p2

(* Séquence simple gauche : p1 puis p2, garde p1 (utile pour parenthèses) *)
let (>--) p1 p2 = p1 ++> fun x -> p2 ++> fun _ -> epsilon_res x

(* Répétition (Star/Many) : renvoie une LISTE de résultats *)
let rec many p = fun l ->
  try
    let (x, l') = p l in
    let (xs, l'') = many p l' in
    (x :: xs, l'')
  with Echec -> ([], l)

(* --------------------------------------------------------------- *)
(* PARTIE 3 : ANALYSEUR LEXICAL ET SYNTAXIQUE (Ex 2.1)             *)
(* --------------------------------------------------------------- *)

(* Gestion des espaces (Ex 2.1.4) *)
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'
let spaces : (unit, char) ranalist = fun l ->
  let rec aux = function x::xs when is_space x -> aux xs | l -> ((), l) in aux l

(* Token : mange les espaces autour d'un parser *)
let token p = spaces --> p >-- spaces

(* Parseurs de base *)
let char_exact c = fun l -> match l with x::xs when x=c -> ((), xs) | _ -> raise Echec
let symbol c = token (char_exact c)

let digit = token (fun l -> match l with
  | c::xs when c >= '0' && c <= '9' -> (int_of_char c - int_of_char '0', xs)
  | _ -> raise Echec)

let variable = token (fun l -> match l with
  | 'a'::xs -> (A, xs) | 'b'::xs -> (B, xs) | 'c'::xs -> (C, xs) | 'd'::xs -> (D, xs)
  | _ -> raise Echec)

(* -- Règles de grammaire récursives -- *)

(* F ::= !F | (E) | Atom *)
let rec atom l =
  ((digit ++> fun n -> epsilon_res (Const n)) +|
   (variable ++> fun v -> epsilon_res (Var v)) +|
   (symbol '(' --> expr >-- symbol ')')) l

and factor l =
  ((symbol '!' --> factor ++> fun e -> epsilon_res (Not e)) +| atom) l

(* T ::= F . F ... (Priorité haute, AND) *)
and term l =
  (factor ++> fun f -> many (symbol '.' --> factor) ++> fun fs ->
   epsilon_res (List.fold_left (fun acc x -> And(acc, x)) f fs)) l

(* E ::= T + T ... (Priorité basse, OR) *)
and expr l =
  (term ++> fun t -> many (symbol '+' --> term) ++> fun ts ->
   epsilon_res (List.fold_left (fun acc x -> Or(acc, x)) t ts)) l

(* Instructions *)
let skip_stmt = token (fun l -> match l with 
  | 's'::'k'::'i'::'p'::xs -> (Skip, xs) | _ -> raise Echec)

let assign_stmt =
  variable ++> fun v -> symbol ':' --> symbol '=' --> expr ++> fun e -> epsilon_res (Assign(v, e))

let rec stmt l = (skip_stmt +| if_stmt +| while_stmt +| assign_stmt) l

(* MODIFICATION EX 2.2.2 : Parsing d'une expression (expr) au lieu d'une variable *)
and if_stmt l =
  (symbol 'i' --> symbol '(' --> expr ++> fun c -> symbol ')' --> (* <-- expr ici *)
   symbol '{' --> program ++> fun p1 -> symbol '}' -->
   symbol '{' --> program ++> fun p2 -> symbol '}' -->
   epsilon_res (If(c, p1, p2))) l

(* MODIFICATION EX 2.2.2 : Parsing d'une expression (expr) au lieu d'une variable *)
and while_stmt l =
  (symbol 'w' --> symbol '(' --> expr ++> fun c -> symbol ')' --> (* <-- expr ici *)
   symbol '{' --> program ++> fun p -> symbol '}' -->
   epsilon_res (While(c, p))) l

(* Programme : suite d'instructions séparées par ; *)
and program l =
  (stmt ++> fun s -> many (symbol ';' --> stmt) ++> fun ss ->
   let rec chain p = function [] -> p | x::xs -> Seq(p, chain x xs) in
   epsilon_res (chain s ss)) l

(* Fonction principale de parsing *)
let parse s =
  try let (ast, _) = program (list_of_string s) in ast
  with Echec -> (Printf.printf "Erreur de syntaxe\n"; Skip)


(* --------------------------------------------------------------- *)
(* PARTIE 4 : INTERPRÉTEUR / SÉMANTIQUE NATURELLE (Ex 2.2.1 + 2.2.2) *)
(* --------------------------------------------------------------- *)

(* L'état est une fonction : Var -> int *)
type state = var -> int

(* État initial : tout à 0 *)
let empty_state : state = fun _ -> 0

(* Mise à jour de l'état : state[v <- n] *)
let update (s : state) (v : var) (n : int) : state =
  fun x -> if x = v then n else s x

(* Outils booléens (0 = Faux, autre = Vrai) *)
let to_bool i = i <> 0
let to_int b = if b then 1 else 0

(* Évaluation des expressions *)
let rec eval_expr (s : state) (e : expr) : int =
  match e with
  | Const n -> n
  | Var v -> s v
  | Not e -> if (eval_expr s e) = 0 then 1 else 0
  | And (e1, e2) -> 
      let v1 = eval_expr s e1 in
      let v2 = eval_expr s e2 in
      to_int (to_bool v1 && to_bool v2)
  | Or (e1, e2) ->
      let v1 = eval_expr s e1 in
      let v2 = eval_expr s e2 in
      to_int (to_bool v1 || to_bool v2)

(* Évaluation des instructions (Sémantique Naturelle) *)
let rec eval_stmt (s : state) (p : prog) : state =
  match p with
  | Skip -> s
  
  | Assign (v, e) ->
      let res = eval_expr s e in
      update s v res

  | Seq (p1, p2) ->
      let s' = eval_stmt s p1 in  (* Exécute p1 *)
      eval_stmt s' p2             (* Puis exécute p2 avec le nouvel état *)

  (* MODIFICATION EX 2.2.2 : Évaluation de l'expression complexe 'c' *)
  | If (c, p1, p2) ->
      let cond_val = eval_expr s c in (* <-- Évaluation explicite *)
      if to_bool cond_val then eval_stmt s p1 else eval_stmt s p2

  (* MODIFICATION EX 2.2.2 : Évaluation de l'expression complexe 'c' *)
  | While (c, p_body) ->
      let cond_val = eval_expr s c in (* <-- Évaluation explicite à chaque tour *)
      if to_bool cond_val then
        let s' = eval_stmt s p_body in
        eval_stmt s' (While(c, p_body))
      else
        s

(* --------------------------------------------------------------- *)
(* PARTIE 5 : TESTS ET EXÉCUTION                                   *)
(* --------------------------------------------------------------- *)

(* Fonction utilitaire pour afficher l'état *)
let print_state (s : state) =
  Printf.printf "  -> État final : A=%d, B=%d, C=%d, D=%d\n" (s A) (s B) (s C) (s D)

(* Fonction principale : Parse une chaine ET l'exécute *)
let run (code : string) =
  Printf.printf "\n[TEST] Programme : \"%s\"\n" code;
  let ast = parse code in
  if ast = Skip && code <> "skip" then 
    Printf.printf "  -> Parsing échoué (ou Skip généré par erreur)\n"
  else begin
    try
      let final_s = eval_stmt empty_state ast in
      print_state final_s
    with Stack_overflow -> 
      Printf.printf "  -> Erreur : Boucle infinie !\n"
  end

(* Jeux de tests complets *)
let () =
  Printf.printf "=== DÉBUT DES TESTS COMPLETS ===\n";

  (* 1. TEST SYNTAXE & PRIORITÉS (Partie 2.1) *)
  (* Priorité ET (.) > OU (+) *)
  (* 1 + 0 . 0 => 1 + (0) => 1 (Vrai) *)
  (* Si priorité fausse : (1+0) . 0 => 1 . 0 => 0 (Faux) *)
  run "a := 1 + 0 . 0"; 

  (* Priorité Parenthèses *)
  (* !(1.0) => !(0) => 1 *)
  run "b := !(1 . 0)";

  (* 2. TEST GESTION DES BLANCS (Partie 2.1.4) *)
  run "c   :=   1 ;    skip"; 

  (* 3. TEST INSTRUCTIONS DE BASE (Partie 2.2.1) *)
  (* Séquence et affectation *)
  run "a := 1; b := a; c := 0";

  (* 4. TEST STRUCTURES DE CONTRÔLE (Partie 2.2.2) *)
  (* IF avec expression complexe *)
  (* Si (a=1 ET b=1) alors c:=1 sinon c:=0. Ici a=0, donc faux. *)
  run "a := 1; b := 1; i( a . b ){ c:=1 }{ c:=0 }"; 

  (* WHILE avec expression complexe *)
  (* Tant que (a ET b) est vrai, on met b à 0 (ce qui arrête la boucle) *)
  run "a := 1; b := 1; w( a . b ){ b := 0; c := 1 }";
  
  Printf.printf "=== FIN DES TESTS ===\n"
