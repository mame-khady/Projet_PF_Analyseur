(* ========================================================================== *)
(* 2. PARTIE PRINCIPALE                            *)
(* ========================================================================== *)

(* Mise à jour des types pour supporter les exercices 2.1.3 et 2.2.2 *)
(* On étend expr pour supporter les opérations booléennes et arithmétiques *)

type var = A | B | C | D;;

(* Extension pour Exercice 2.1.3  : Expressions complexes *)
type expr =
  | Const of int
  | Var of var
  | Add of expr * expr  (* Pour '+' *)
  | And of expr * expr  (* Pour '.' *)
  | Not of expr         (* Pour '!' *)
;;

type prog =
  | Skip
  | Assign of var * expr
  | Seq of prog * prog
  | If of expr * prog * prog  (* Condition est une expr maintenant *)
  | While of expr * prog
;;

(* -------------------------------------------------------------------------- *)
(* 2.1 Implémentation de l'analyseur (Parser)               *)
(* -------------------------------------------------------------------------- *)

(* --- Outils : Combinateurs de Parsers (Technique recommandée) --- *)

(* Un parser prend une liste de char et rend (résultat, reste) ou rien *)
type 'a parser = char list -> ('a * char list) option;;

(* Retourne une valeur sans consommer *)
let return (x : 'a) : 'a parser = fun stream -> Some (x, stream);;

(* Échec du parsing *)
let fail : 'a parser = fun _ -> None;;

(* Lit un caractère précis *)
let p_char (c : char) : char parser = fun stream ->
  match stream with
  | x :: xs when x = c -> Some (c, xs)
  | _ -> None
;;

(* "Bind" : enchaîne deux parsers *)
let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser = fun stream ->
  match p stream with
  | None -> None
  | Some (res, rest) -> f res rest
;;

(* Choix : essaie p1, si ça rate, essaie p2 *)
let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser = fun stream ->
  match p1 stream with
  | Some r -> Some r
  | None -> p2 stream
;;

(* Exercice 2.1.4 (Facultatif) : Gestion des blancs *)
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r';;

let rec spaces stream =
  match stream with
  | c :: rest when is_space c -> spaces rest
  | _ -> stream
;;

(* Fonction "token" : applique un parser et ignore les espaces après *)
let token (p : 'a parser) : 'a parser = fun stream ->
  p (spaces stream)
;;

(* --- Analyseurs spécifiques au langage WHILE --- *)

(* Parse une variable : a, b, c, d *)
let parse_var : var parser = fun stream ->
  match spaces stream with (* on ignore les espaces avant *)
  | 'a'::rest -> Some (A, rest)
  | 'b'::rest -> Some (B, rest)
  | 'c'::rest -> Some (C, rest)
  | 'd'::rest -> Some (D, rest)
  | _ -> None
;;

(* Parse une constante : 0 ou 1 *)
let parse_const : expr parser = fun stream ->
  match spaces stream with
  | '0'::rest -> Some (Const 0, rest)
  | '1'::rest -> Some (Const 1, rest)
  | _ -> None
;;

(* Exercice 2.1.3  : Parser d'expressions avec priorités *)
(* Grammaire :
   E ::= T '+' E | T
   T ::= F '.' T | F
   F ::= '!' F | A | '(' E ')'
*)

let rec parse_expr stream = parse_E stream

and parse_E stream =
  ( 
    (parse_T >>= fun t ->
     token (p_char '+') >>= fun _ ->
     parse_E >>= fun e ->
     return (Add(t, e)))
    <|> parse_T
  ) stream 

and parse_T stream =
  ( 
    (parse_F >>= fun f ->
     token (p_char '.') >>= fun _ ->
     parse_T >>= fun t ->
     return (And(f, t)))
    <|> parse_F
  ) stream 

and parse_F stream =
  ( 
    (token (p_char '!') >>= fun _ ->
     parse_F >>= fun f ->
     return (Not f))
    <|>
    (token (p_char '(') >>= fun _ ->
     parse_expr >>= fun e ->
     token (p_char ')') >>= fun _ ->
     return e)
    <|>
    (parse_var >>= fun v -> return (Var v))
    <|>
    parse_const
  ) stream 
;;

(* Parser des instructions (Stmt) *)
(* Note: On utilise 'let rec' car If et While contiennent des programmes *)
(* --- BLOC 2 : Parsers d'Instructions (CORRIGÉ AVEC PARENTHÈSES GLOBALES) --- *)

let rec parse_stmt stream =
  (
    ( (* Assign *)
      parse_var >>= fun v ->
      token (p_char ':') >>= fun _ ->
      token (p_char '=') >>= fun _ ->
      parse_expr >>= fun e ->
      return (Assign (v, e))
    )
    <|>
    ( (* While avec appel à parse_prog *)
      token (p_char 'w') >>= fun _ ->
      token (p_char '(') >>= fun _ ->
      parse_expr >>= fun e ->
      token (p_char ')') >>= fun _ ->
      token (p_char '{') >>= fun _ ->
      parse_prog >>= fun p ->  (* <--- Appel récursif ici *)
      token (p_char '}') >>= fun _ ->
      return (While (e, p))
    )
    <|>
    ( (* If avec appel à parse_prog *)
      token (p_char 'i') >>= fun _ ->
      token (p_char '(') >>= fun _ ->
      parse_expr >>= fun e ->
      token (p_char ')') >>= fun _ ->
      token (p_char '{') >>= fun _ ->
      parse_prog >>= fun p1 ->
      token (p_char '}') >>= fun _ ->
      token (p_char '{') >>= fun _ ->
      parse_prog >>= fun p2 ->
      token (p_char '}') >>= fun _ ->
      return (If (e, p1, p2))
    )
    <|>
    ( (* Skip *)
       return Skip
    )
  ) stream

(* 'and' colle cette fonction à la précédente *)
and parse_prog stream =
  (
    parse_stmt >>= fun s1 ->
    (
      (token (p_char ';') >>= fun _ ->
       parse_prog >>= fun s2 ->
       return (Seq (s1, s2)))
      <|>
      return s1
    )
  ) stream
;; 
(* Fonction utilitaire pour lancer le parser sur un string *)
let analyser (s : string) : prog =
  let char_list = List.init (String.length s) (String.get s) in
  match parse_prog char_list with
  | Some (res, rest) ->
      (* On utilise la fonction 'spaces' pour manger les \n et espaces restants *)
      (match spaces rest with
       | [] -> res (* Succès : il ne reste que du vide ou des espaces *)
       | _ -> failwith "Erreur syntaxe: reste non lu (caractères inattendus à la fin)")
  | None -> failwith "Erreur syntaxe: échec"
;;

(* -------------------------------------------------------------------------- *)
(* Exercice 2.1.2 : Tests Analyseur                         *)
(* -------------------------------------------------------------------------- *)

(* Test 1 : Programme simple *)
let test_prog1 = "a:=1; b:=0";;
let ast1 = analyser test_prog1;;

(* Test 2 : Exemple complexe avec espaces (grâce à 2.1.4) *)
let test_prog2 = "
  a:=1;
  b:=1;
  c:=1;
  w(a){
    i(c){
       c:=0; a:=b
    }{
       b:=0; c:=a
    }
  }
";;
let ast2 = analyser test_prog2;;


(* ========================================================================== *)
(* 2.2 EXECUTION (INTERPRÉTEUR)                         *)
(* ========================================================================== *)

(* Exercice 2.2.1 : Représentation de l'état *)
(* On représente l'état comme une fonction qui associe une valeur à une variable *)
(* C'est proche de la notation mathématique 'sigma' *)

type etat = var -> int;;

(* État initial : tout le monde vaut 0 *)
let etat_vide : etat = fun _ -> 0;;

(* Mise à jour de l'état : update s v n *)
let update (s : etat) (v : var) (n : int) : etat =
  fun x -> if x = v then n else s x
;;

(* Helper pour lire l'état (juste pour le debug) *)
let print_etat (s : etat) =
  Printf.printf "A=%d, B=%d, C=%d, D=%d\n" (s A) (s B) (s C) (s D)
;;

(* Exercice 2.2.2 : Évaluation des expressions complètes (avec +, ., !) *)
(* Rappel : En WHILEb--, true c'est 1 (ou !=0) et false c'est 0 *)

let rec eval_expr (e : expr) (s : etat) : int =
  match e with
  | Const n -> n
  | Var v -> s v
  | Add (e1, e2) ->
      let v1 = eval_expr e1 s in
      let v2 = eval_expr e2 s in
      if (v1 + v2) > 0 then 1 else 0 (* Disjonction booléenne + *)
  | And (e1, e2) ->
      let v1 = eval_expr e1 s in
      let v2 = eval_expr e2 s in
      if (v1 * v2) > 0 then 1 else 0 (* Conjonction booléenne . *)
  | Not e1 ->
      if (eval_expr e1 s) = 0 then 1 else 0
;;

(* Exercice 2.2.1 suite : Exécution du programme (Sémantique Naturelle) *)

let rec executer (p : prog) (s : etat) : etat =
  match p with
  | Skip -> s
  | Assign (v, e) ->
      let val_e = eval_expr e s in
      update s v val_e
  | Seq (p1, p2) ->
      let s1 = executer p1 s in
      executer p2 s1
  | If (cond, p1, p2) ->
      if (eval_expr cond s) <> 0 then
        executer p1 s
      else
        executer p2 s
  | While (cond, corps) ->
      if (eval_expr cond s) = 0 then
        s (* Condition fausse, on sort *)
      else
        let s1 = executer corps s in (* Un tour de boucle *)
        executer (While (cond, corps)) s1 (* On recommence *)
;;


(* -------------------------------------------------------------------------- *)
(* TESTS D'EXECUTION                              *)
(* -------------------------------------------------------------------------- *)

(* Test simple : échange de variables *)
let prog_test = analyser "a:=1; b:=0; c:=a; a:=b; b:=c";;
let etat_final = executer prog_test etat_vide;;

print_endline "--- Résultat exécution Test 1 ---";;
print_etat etat_final;;
(* Attendu : A=0, B=1 (échange), C=1 *)

(* Test Expressions complexes  *)
(* a := 1 + 0 (donc 1 ou true) *)
(* b := !a (donc 0 ou false) *)
let prog_complex = analyser "a:=1+0; b:=!a";;
let etat_complex = executer prog_complex etat_vide;;

print_endline "--- Résultat exécution Expressions Complexes ---";;
print_etat etat_complex;;
(* Attendu : A=1, B=0 *)

let etat_boucle = executer ast2 etat_vide;;
print_etat etat_boucle;;

(* Attendu : A=0, B=0 *)
