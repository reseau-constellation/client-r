# Exécuter du code dans le contexte d'un client Constellation, et fermer le client par la suite. Nécessite qu'un serveur Constellation soit déjà activé sur le \`port\` spécifié.

Exécuter du code dans le contexte d'un client Constellation, et fermer
le client par la suite. Nécessite qu'un serveur Constellation soit déjà
activé sur le \`port\` spécifié.

## Usage

``` r
avecClient(code, port, codeSecret)
```

## Arguments

- code:

  Le code à exécuter. Ce code doit être une fonction qui prend le
  \`client\` Constellation comme unique paramètre.

- port:

  Le port du serveur déjà ouvert.

- codeSecret:

  Le code secret pour se connecter au serveur.

## Value

Le résultat de la fonction \`code\`.
