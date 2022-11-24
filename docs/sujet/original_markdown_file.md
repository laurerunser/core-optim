---
created: 2022-11-01
updated: 2022-11-03
---
# Moteur d'optimisation de programmes fonctionnelles

Contact: [Pierre-Evariste Dagand](https://evr.ist)

## Introduction g�n�rale

La programmation fonctionnelle offre la possibilit� d'�crire des
programmes de haut-niveau. Les programmeurs fonctionnels peuvent ainsi
communiquer plus naturellement leurs intentions � la fois au
compilateur mais �galement aux mainteneurs futurs, dans un style
efficace et �pur� proche des math�matiques. En cons�quence, un
compilateur de langage fonctionnel doit �tre en mesure de
transformer agressivement les programmes afin de permettre leur
ex�cution efficace.

Dans ce projet long, nous proposons d'�tudier et d'impl�menter un
moteur d'optimisation pour un fragment du langage Core de
[GHC](https://www.haskell.org/ghc/). Il s'agit d'un langage pure (sans
effet de bord), offrant des types alg�briques, du typage explicite
de second ordre et une s�mantique paresseuse.

L'optimiseur fut le sujet de la 
[th�se de Andre Santos](http://theses.gla.ac.uk/id/eprint/74568)
ainsi que 3 publications de conf�rence :

  - [Let-floating: Moving Bindings to Give Faster Programs; Peyton Jones, Partain et Santos](https://doi.org/10.1145/232627.232630)
  - [A Transformation-Based Optimiser for Haskell, Peyton Jones et Santos](https://doi.org/10.1016/S0167-6423(97)00029-4)
  - [Secrets of the Glasgow Haskell Compiler inliner; Peyton Jones et Marlow](https://doi.org/10.1017/S0956796802004331)

L'objectif est de r�duire le nombre de d�clarations (`let`) ainsi que
le nombre de `match` tout en �vitant une explosion combinatoire de la
taille du code g�n�r�. L'optimiseur fonctionne de source � source, en
pr�servant le typage : il consomme un programme Core bien typ� et
produit un programme Core bien typ�.

## Objectifs

L'objectif de ce projet long est d'apprendre les diff�rentes
techniques (et astuces) d'impl�mentation des langages fonctionnels. Il
s'agira en particulier de mod�liser une syntaxe avec lieurs (`let`,
`lambda`), d'impl�menter un algorithme de typage et d'effectuer des
transformations symboliques (optimisations) sur les termes de ce
langage. Le travail sera effectu� en OCaml.

Si le temps le permet, on abordera des optimisations avanc�es, en lien
avec le flot de contr�le des programmes fonctionnels et bas� sur la
notion de ["join point"](https://doi.org/10.1145/3062341.3062380).

## Calendrier

Le calendrier pr�visionnel est divis� en 3 phases d'approximativement 3 semaines
chacunes :

 - mod�lisation du langage source en OCaml
 - impl�mentation d'un v�rificateur de type
 - impl�mentation de transformations de programmes (inlining, let-floating)

## R�f�rences

  - [Compilation by transformation for non-strict functional languages; Santos](http://theses.gla.ac.uk/id/eprint/74568)
  - [Let-floating: Moving Bindings to Give Faster Programs; Peyton Jones, Partain et Santos](https://doi.org/10.1145/232627.232630)
  - [A Transformation-Based Optimiser for Haskell, Peyton Jones et Santos](https://doi.org/10.1016/S0167-6423(97)00029-4)
  - [Secrets of the Glasgow Haskell Compiler inliner; Peyton Jones et Marlow](https://doi.org/10.1017/S0956796802004331)
  - [Compiling without continuations; Maurer, Downen, Ariola et Peyton Jones](https://doi.org/10.1145/3062341.3062380)
  - [Compiling with continuations, or without? whatever; Cong, Osvald, Essertel et Rompf](https://doi.org/10.1145/3341643)
