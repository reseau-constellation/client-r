# Exécuter du code dans le contexte d'un client Constellation, et fermer le client et le serveur par la suite.

Exécuter du code dans le contexte d'un client Constellation, et fermer
le client et le serveur par la suite.

## Usage

``` r
avecClientEtServeur(code, ...)
```

## Arguments

- code:

  Le code à exécuter. Ce code doit être une fonction qui prend le
  \`client\` Constellation comme unique paramètre.

- ...:

  Paramètres qui seront passés directement à
  \`constellationR::avecServeur\`.

## Value

Le résultat de la fonction \`code\`.
