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

- le fichier main contient des tests de développement
- la fonction `alpha` qui vérifie la α-conversion de deux termes est efficace grace à
  l'utilisation d'un environnement de substitution
- ajout commande `read` pour charger le contenu d'un fichier (voir [exemple](./proofs/2-pred.proof))
- commande `check-val`
