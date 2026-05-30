# Essaie de deviner de quel type est une fonction Constellation.

Essaie de deviner de quel type est une fonction Constellation.

## Usage

``` r
résoudreTypeFonction(nomFonction)
```

## Arguments

- nomFonction:

  Le nom de la fonction selon la documentation Constellation
  (https://docu.réseau-constellation.ca), par exemple, "obtIdCompte" ou
  "variables.créerVariable"

## Value

Le type de fonction, soit "suivi", "action", ou "recherche".

## Examples

``` r
typeFonction <- résoudreTypeFonction("recherche.rechercherVariablesSelonNom")
typeFonction == "recherche"
#> [1] TRUE
```
