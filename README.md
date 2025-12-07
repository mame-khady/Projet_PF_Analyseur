# Projet : Analyse syntaxique et sémantique - WHILEb

## Structure du projet
- Dossier "LT/" :
Contient les preuves formelles en Rocq (Coq) :

TD_LT_6_7_8.v : Fichier principal regroupant les preuves sur :

Partie 2.3 :La sémantique naturelle (SN) - TD6 

Partie 3.1 :La sémantique opérationnelle structurée (SOS) - TD7

Partie 3.9 :La compilation d'expressions arithmétiques - TD8


- Dossier "PF/" :

Contient l'implémentation OCaml du projet, organisée en trois parties :
### Partie 1 : Définitions théoriques

+ partie1.ml : Définitions des types OCaml pour représenter les programmes WHILEb⁻⁻ (AST, expressions, instructions)

### Partie 2 : Implémentation principale

+ anacomb.ml : Bibliothèque de combinateurs d'analyseurs syntaxiques (parser combinators)

Fonctions de base : terminal, epsilon, epsilon_res
Combinateurs : -->, ++>, |||
Utilitaire indispensable pour les analyseurs des parties suivantes


+ Partie2_1_1.ml : Analyseur syntaxique pour WHILEb⁻⁻ (version simplifiée)


+ Partie2_1_3.ml : Analyseur syntaxique pour WHILEb (version complète)

+ Partie2_1_4.ml : Analyseur amélioré avec gestion des espaces

+ Partie2_1_final.ml : Analyseur complet (WHILEb avec Priorités & Espaces)


+ Partie2_2.ml : Interpréteur WHILEb basé sur la sémantique naturelle (SN)


### Partie 3 : Extensions optionnelles

+ Partie3_2.ml : Analyse lexicale et syntaxique avancée

+ Partie3_3.ml : Optimisation avec listes paresseuses (lazy lists)

+ Partie3_4.ml : Interpréteur basé sur la sémantique SOS (Small-Step Operational Semantics)



## Comment exécuter le projet OCaml

### Prérequis

- OCaml (version 4.08 ou supérieure recommandée)

- Compilateur OCaml (ocamlc ou ocamlopt)

- Un éditeur de texte (Emacs recommandé pour les systèmes Linux)


### Compilation

**Méthode 1 : Ligne de commande**

Compiler un fichier individuel :
```bash
# Depuis le dossier PF/
ocamlc -o test Partie1/partie1.ml Partie2/anacomb.ml Partie2/Partie2_1_1.ml
```

**Méthode 2 : Avec Emacs**

1. **Ouvrir un fichier OCaml dans Emacs :**
```bash
   emacs Partie1/partie1.ml
```

2. **Vérifier que le mode Tuareg est actif :**
    - En bas de la fenêtre Emacs, vous devriez voir `Tuareg` dans la barre de mode
    - Si ce n'est pas le cas, tapez : `M-x tuareg-mode` (M-x = Alt+x ou ESC puis x)

3. **Compiler depuis Emacs :**
    - Appuyez sur `C-c C-b` (Ctrl+c puis Ctrl+b)
    - Cela compile le buffer courant
    - Les erreurs de compilation s'affichent dans un buffer séparé

**Commandes Emacs utiles :**
- `C-c C-b` : Compiler le fichier courant
- `C-c C-r` : Évaluer une région sélectionnée dans le toplevel OCaml
- `C-c C-e` : Évaluer une expression/phrase
- `C-c C-t` : Afficher le type d'une expression (avec Merlin)
- `C-x C-s` : Sauvegarder le fichier
- `C-x C-c` : Quitter Emacs

# Charger les fichiers

#use "Partie1/partie1.ml";;
#use "Partie2/anacomb.ml";;
#use "Partie2/Partie2_1_1.ml";;
#use "Partie2/Partie2_2.ml";;

# Parties Traitées 

Partie 1 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 2.1 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 2.2 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 2.3 :Chaimae Bouti - mam Khady - Salma Kamal

Partie 2.4 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 3.1 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 3.2 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 3.3 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 3.4 : Chaimae Bouti - mam Khady - Salma Kamal

Partie 3.9 :Chaimae Bouti - mam Khady - Salma Kamal



