
# constellationR

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/reseau-constellation/client-r/workflows/R-CMD-check/badge.svg)](https://github.com/reseau-constellation/client-r/actions)
[![R-CMD-check](https://github.com/reseau-constellation/client-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/reseau-constellation/client-r/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/github/reseau-constellation/client-r/graph/badge.svg?token=U2MUE2ZLGO)](https://codecov.io/github/reseau-constellation/client-r)
<!-- badges: end -->

`constellationR` est un paquet R pour interagir avec le logiciel Constellation pour le partage de données scientifiques distribuées.

## Installation

Vous pouvez installer `constellationR` de [GitHub](https://github.com/) ainsi :

``` r
# install.packages("devtools")
devtools::install_github("reseau-constellation/client-r")
```

## Utilisation
Vous pouvez effectuer des actions ainsi :

``` r
library(constellationR)

constellationR::avecClientEtServeur(
  function (client) {
    # Accéder l'identifiant du compte
    idCompte <- client$appeler("obtIdCompte")
    
    # Créer une nouvelle base de données
    idBd <- client$appeler(
      "bds.créerBd", 
      list(licence="ODbl-1_0")
    )
  }
)

```

Vous pouvez également suivre des données du réseau Constellation :
``` r
library(constellationR)

constellationR::avecClientEtServeur(
  function (client) {
    oublier <- client$appeler(
      "bds.suivreNomsBd",
      list(
        idBd = idBd,
        f = print
      )
    )
    
    Sys.sleep(2)
    # Arrêter le suivi après 2 secondes
    oublier()
  }
)
```

Si vous ne voulez pas suivre les données, mais seulement obtenir leur valeur au moment que la fonction est invoquée, vous n'avez qu'à omettre le paramètre de fonction de suivi :

``` r
library(constellationR)

constellationR::avecClientEtServeur(
  function (client) {
    nomsBd <- client$appeler(
      "bds.suivreNomsBd",
      list(
        idBd = idBd
      )
    )
  }
)
```

C'est là même chose pour les fonctions de recherche :

``` r
library(constellationR)

constellationR::avecClientEtServeur(
  function (client) {
    variablesTrouvées <- NULL
    f <- function(résultats) {
      variablesTrouvées <<- sapply(résultats, (\(x) x$id))
    }
    retour <- client$rechercher(
      "recherche.rechercherVariablesSelonNom",
      list(nomVariable="oiseaux", nRésultatsDésirés = 10, f = f)
    )

    idVariableAudio <- client$action(
      "variables.créerVariable", list(catégorie="audio")
    )

    client$action(
      "variables.sauvegarderNomVariable",
      list(idVariable=idVariableAudio, langue="fr", nom="Audio oiseaux")
    )

    idVariableNom <- client$action(
      "variables.créerVariable", list(catégorie="chaîne")
    )

    client$action(
      "variables.sauvegarderNomVariable",
      list(idVariable=idVariableNom, langue="fr", nom="Nom oiseau")
    )
    
    retour$fChangerN(1)
    
    Sys.sleep(2)
    
    retour$fChangerN(4)
    
    Sys.sleep(2)
    
    retour$fOublier()
  }
)
```

### Serveur existant
Si vous avez déjà lancé un serveur Constellation (p. ex., dans l'interface graphique ou bien à travers un autre processus), vous pouvez vous connecter directement à celui-ci.
```r
library(constellationR)

// Le numéro du port sur lequel vous avez lancé Constellation
port <- 5003

constellationR::avecClient(
  function(client) {
    // Faire quelque chose avec le client...
  },
  port = port
)

```
