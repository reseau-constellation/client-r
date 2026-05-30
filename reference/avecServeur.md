# Exécuter du code dans le contexte d'un serveur Constellation, et fermer le serveur par la suite.

Exécuter du code dans le contexte d'un serveur Constellation, et fermer
le serveur par la suite.

## Usage

``` r
avecServeur(code, ...)
```

## Arguments

- code:

  Le code à exécuter. Ce code doit être une fonction qui prend le
  \`port\` du serveur comme unique paramètre.

- ...:

  Arguments qui seront passés directement à
  \`constellationR::lancerServeur\`.

## Value

Le résultat de la fonction \`code\`.
