## Partie 2 : Types dépendants

### Usage

- Lancer l'interaction automatique
  ```
  $ make
  $ cat proofs/bool.proof | ./loop.exe
  ```

- Interaction
  ```
  $ make
  $ ./loop.exe
  ```
  ou
  ```
  $ dune exec src/loop.exe
  ```


### Remarques

- Le projet est organisé en plusieurs fichiers :
  + `loop.ml` : point d'entrée principal qui définit la boucle d'interaction
  + `alpha_beta.ml` : gestion des termes (fonctions sur les variables libres, de substitution, normalisation et αβ-équivalence)
  + `parselex.ml` : isolation du parser/lexer fourni dans le sujet (je ne l'ai pas re-codé en ocamllex/menhir dans cette partie du projet pour l'instant)
  + `utils.ml` : bazar pour syntaxe
  + `types.ml` : définition du types pour les λΠ-termes et définition d'un printer sur le type
  + `main.ml` : tests écrit à la main (ignorer ces fichiers)

- le fichier main contient des tests de développement

- la fonction `alpha` qui vérifie la α-conversion de deux termes est efficace grâce à
  l'utilisation d'un environnement de substitution

- ajout de commandes :
  + `check_val` : type un terme et vérifie l'αβ-égalité avec valeur donnée (voir [exemple](./proofs/2-maths.proof) ligne 8 et 9)
  + `read` : charge le contenu d'un fichier (voir [exemple](./proofs/2-pred.proof)), cela évite de redéfinir des fonctions, on peut alors écrire une bibliothèque `math` par exemple.
