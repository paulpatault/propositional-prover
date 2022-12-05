## Partie 1 : prouveur interactif

### Usage

- Prouver un fichier
  ```
  $ dune exec src/proove.exe -- proofs/xxx.proof [ajouter arg pour "pas à pas"]
  ```
  Si on donne un arg en plus (n'importe quoi),
  le prouveur attend que l'on entre qqchose pour avancer.

- Interaction
  ```
  $ dune exec src/proove.exe
  ```
### Remarques

- Tous les fichiers de preuves passent.
- J'ai re-codé le lexer/parser avec ocamllex et menhir (cela ajoute donc une dépendance menhir `opam install menhir`).
- "pas à pas" correspond à une attente d'input dans la boucle interactive (permet de lire pas à pas une preuve déjà écrite).

### Features

- `use [filename]` : donner un ficher et appliquer les tactiques s'y trouvant (exemple d'utilisation dans le fichier [meta.proof](proofs/meta.proof))
- `trustme` : commande de triche qui typecheck avec un `magic` à la OCaml (exemple d'utilisation dans le fichier [triche.proof](proofs/triche.proof))
- `back` : annule la dernière commande (WIP: ne fonctionne pas sur toutes les tactiques)
