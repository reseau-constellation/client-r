# Lancer un serveur Constellation local

Lancer un serveur Constellation local

## Usage

``` r
lancerServeur(port = NULL, dossier = NULL, exe = "constl")
```

## Arguments

- port:

  Le numéro du port sur lequel le port sera connecté

- dossier:

  Le dossier du compte Constellation à utiliser (optionnel)

- exe:

  La commande pour lancer Constellation. Uniquement nécessaire pour une
  installation de Constellation non standard

## Value

Le numéro de port sur lequel le le serveur écoute désormais, et une
fonction à appeler pour fermer le serveur
