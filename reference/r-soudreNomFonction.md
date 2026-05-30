# Résout le nom de la fonction en séparant ses composantes.

Résout le nom de la fonction en séparant ses composantes.

## Usage

``` r
résoudreNomFonction(nomFonction)
```

## Arguments

- nomFonction:

  Le nom de la fonction; par exemple, "profil.sauvegarderNoms"

## Value

Le nom décomposé de la fonction en format vecteur

## Examples

``` r
nomRésolu <- résoudreNomFonction("profil.sauvegarderNoms")
nomRésolu == c("profil", "sauvegarderNoms")
#> [1] TRUE TRUE
```
