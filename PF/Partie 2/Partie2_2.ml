(* ============================================================ *)
(* PRÉ-REQUIS : GESTION DE LA MÉMOIRE                           *)
(* ============================================================ *)
type state = var -> int

let init_state : state = fun v -> 0

let update (s : state) (x : var) (v : int) : state =
  fun y -> if y = x then v else s y

let dump_state (s : state) =
  Printf.printf "Etat : [A=%d, B=%d, C=%d, D=%d]\n" (s A) (s B) (s C) (s D)


(* ============================================================ *)
(* EXERCICE 2.2.1 : INTERPRÉTEUR RESTREINT (WHILEb--)           *)
(* Objectif : Ne gérer que 0, 1 et les Variables.               *)
(* ============================================================ *)

(* Évaluation Simple  *)
let rec eval_simple (s : state) (e : expr) : int =
  match e with
  | Const n -> n
  | Var v   -> s v
  (* Si on tombe sur autre chose, erreur *)
  | _ -> failwith "Erreur 2.2.1 : Expression trop complexe pour WHILEb--"

(* Exécution "Simple" *)
let rec exec_simple (s : state) (p : prog) : state =
  match p with
  | Skip -> s
  | Assign(v, e) -> update s v (eval_simple s e)
  | Seq(p1, p2) -> exec_simple (exec_simple s p1) p2
  | If(c, p1, p2) -> 
      if eval_simple s c <> 0 then exec_simple s p1 else exec_simple s p2
  | While(c, p) ->
      if eval_simple s c <> 0 then 
        exec_simple (exec_simple s p) (While(c, p))
      else s

(* TEST 2.2.1 *)
let test_simple () =
  Printf.printf "\n--- Test 2.2.1  ---\n";
  (* Ce code simple DOIT passer *)
  let code_ok = "a := 1 ; i(a) { b := 1 } { b := 0 }" in
  let ast = parse code_ok in
  let s = exec_simple init_state ast in
  dump_state s;

  (* Ce code complexe DOIT échouer dans exec_simple *)
  Printf.printf "Tentative code complexe sur moteur simple...\n";
  try 
    let _ = exec_simple init_state (parse "a := 1+1") in ()
  with Failure msg -> Printf.printf "[SUCCES] L'interpreteur a rejete le code : %s\n" msg
;;


(* ============================================================ *)
(* EXERCICE 2.2.2 : INTERPRÉTEUR COMPLET (WHILEb)               *)
(* Objectif : Gérer Not (!), And (.), Or (+)                    *)
(* ============================================================ *)

(* Évaluation "Complète" *)
let rec eval_complet (s : state) (e : expr) : int =
  match e with
  | Const n -> n
  | Var v   -> s v
  (* Extensions 2.2.2 *)
  | Not e -> if eval_complet s e = 0 then 1 else 0
  | And(e1, e2) -> if (eval_complet s e1 <> 0) && (eval_complet s e2 <> 0) then 1 else 0
  | Or(e1, e2)  -> if (eval_complet s e1 <> 0) || (eval_complet s e2 <> 0) then 1 else 0

(* Exécution "Complète" (utilise eval_complet) *)
let rec exec_complet (s : state) (p : prog) : state =
  match p with
  | Skip -> s
  | Assign(v, e) -> update s v (eval_complet s e)
  | Seq(p1, p2) -> exec_complet (exec_complet s p1) p2
  | If(c, p1, p2) -> 
      if eval_complet s c <> 0 then exec_complet s p1 else exec_complet s p2
  | While(c, p) ->
      if eval_complet s c <> 0 then 
        exec_complet (exec_complet s p) (While(c, p))
      else s

(* TEST 2.2.2 *)
let test_complet () =
  Printf.printf "\n--- Test 2.2.2 (Complet) ---\n";
  (* Le code complexe passe ici *)
  let code = "a := 1 ; b := 0 ; c := a + b . !b" in (* c = 1 OR (0 AND 1) -> 1 *)
  let ast = parse code in
  let s = exec_complet init_state ast in
  dump_state s
;;

(* Lancement des deux batteries de tests *)
test_simple ();;
test_complet ();;
