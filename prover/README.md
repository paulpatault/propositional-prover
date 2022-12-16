## Partie 1 : prouveur interactif

### Usage

- Prouver un fichier (1)
  ```
  $ dune exec src/proove.exe -- proofs/xxx.proof [ajouter arg pour "pas à pas"]
  ```
  Si on donne un argument en plus (n'importe quoi),
  le prouveur attend que l'on entre quelque chose pour avancer.

- Interaction
  ```
  $ dune exec src/proove.exe
  ```

### Remarques

- Tous les fichiers de preuves passent correctement avec la commande (1).
- J'ai re-codé un lexer/parser avec ocamllex et menhir (cela ajoute donc une dépendance menhir `opam install menhir`).
- "pas à pas" correspond à une attente d'input dans la boucle interactive (permet de lire pas à pas une preuve déjà écrite).

- Le projet est organisé en plusieurs fichiers :
  + `proove.ml` : point d'entrée principal
  + `printer.ml` : définitions des fonctions d'affichage
  + `proving.ml` : boucle d'interaction
  + `stlc.ml` : gestion des types : inférence et type checking
  + `ulexer.mll` : générateur de lexer avec ocamllex
  + `uparser.mly` : générateur de parser avec menhir
  + `types.ml` : définition de l'AST de termes
  + `main.ml` et `tests.ml` : tests écrit à la main (ignorer ces fichiers)

### Additions personnelles

- `use [filename]` : donner un ficher et appliquer les tactiques s'y trouvant (exemple d'utilisation dans le fichier [meta.proof](proofs/meta.proof))
- `trustme` : commande de triche qui typecheck avec un `magic` à la OCaml (exemple d'utilisation dans le fichier [triche.proof](proofs/triche.proof))
- `back` : annule la dernière commande (WIP: ne fonctionne pas encore sur toutes les tactiques)
