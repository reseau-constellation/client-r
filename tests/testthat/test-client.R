avecClientEtServeurTest <- function(code) {
  id <- uuid::UUIDgenerate()
  dossierBase <- file.path(tempdir(), id)

  dossierOrbite <- file.path(dossierBase, "orbite")
  dossierSFIP <- file.path(dossierBase, "sfip")

  # Effacer le dossier temporaire une fois qu'on a fini
  on.exit(unlink(dossierBase, recursive = TRUE), add = TRUE)

  résultat <- constellationR::avecClientEtServeur(
    code,
    orbite = dossierOrbite,
    sfip = dossierSFIP,
  )
  return(résultat)
}


avecClientEtServeurTest(
  function (client) {

    testthat::test_that("Actions", {
      idCompte <- client$action("obtIdCompte")
      testthat::expect_equal(class(idCompte), "character")
      testthat::expect_gt(nchar(idCompte), 0)
    })

    testthat::test_that("Suivi", {
      nomsProfil <- NULL
      oublier <- client$suivre(
        "profil.suivreNoms",
        list(f = function (noms) {
          nomsProfil <<- noms
        }),
      )
      client$action(
        "profil.sauvegarderNom", list(langue="fr", nom="C'est moi ! :)")
      )

      retry::wait_until(
        identical(nomsProfil, list(fr="C'est moi ! :)")),
        timeout = 5
      )

      testthat::expect_equal(nomsProfil, list(fr="C'est moi ! :)"))
    })

    testthat::test_that("Suivi une fois", {
      nomsProfil <- client$suivre("profil.suivreNoms")
      testthat::expect_equal(nomsProfil, list(fr="C'est moi ! :)"))
    })

    testthat::test_that("Recherche", {
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

      retry::wait_until(length(variablesTrouvées) > 1, timeout=5)
      testthat::expect_true(
        all(c(idVariableNom, idVariableAudio) %in% variablesTrouvées)
      )

      retour$fChangerN(1)
      retry::wait_until(length(variablesTrouvées) <= 1, timeout=5)
      testthat::expect_true(idVariableAudio %in% variablesTrouvées)

      retour$fChangerN(3)
      retry::wait_until(length(variablesTrouvées) > 1, timeout=5)
      testthat::expect_true(
        all(c(idVariableNom, idVariableAudio) %in% variablesTrouvées)
      )

      retour$fOublier()
    })

    testthat::test_that("Recherche une fois", {
      variablesTrouvées <- client$rechercher(
        "recherche.rechercherVariablesSelonNom",
        list(nomVariable="oiseaux", nRésultatsDésirés = 10)
      )

      retry::wait_until(length(variablesTrouvées) > 1, timeout = 5)
      testthat::expect_true(all(nchar(variablesTrouvées) > 0))
    })

    testthat::test_that("Appeler action", {
      idDispositif <- client$appeler("obtIdDispositif")
      testthat::expect_equal(class(idDispositif), "character")
      testthat::expect_gt(nchar(idDispositif), 0)
    })

    testthat::test_that("Appeler suivi", {
      nomsProfil <- client$appeler("profil.suivreNoms")
      testthat::expect_equal(nomsProfil, list(fr="C'est moi ! :)"))
    })

    testthat::test_that("Appeler recherche", {
      variablesTrouvées <- NULL
      f <- function(résultats) {
        variablesTrouvées <<- sapply(résultats, (\(x) x$id))
      }
      retour <- client$appeler(
        "recherche.rechercherVariablesSelonNom",
        list(nomVariable="oiseau", nRésultatsDésirés = 10, f = f)
      )
      retry::wait_until(length(variablesTrouvées) > 1, timeout = 5)
      testthat::expect_true(all(nchar(variablesTrouvées) > 0))
    })

    testthat::test_that("Obtenir données tableau", {
      idBd <- client$appeler(
        "bds.créerBd",
        list(licence="ODbl-1_0")
      )
      idTableau <- client$appeler(
        "bds.ajouterTableauBd",
        list(idBd=idBd)
      )
      idVar <- client$appeler(
        "variables.créerVariable",
        list(catégorie="numérique")
      )
      idVarC <- client$appeler(
        "variables.créerVariable",
        list(catégorie="chaîneNonTraduisible")
      )
      idCol <- client$appeler(
        "tableaux.ajouterColonneTableau",
        list(idTableau=idTableau, idVariable=idVar)
      )
      idColC <- client$appeler(
        "tableaux.ajouterColonneTableau",
        list(idTableau=idTableau, idVariable=idVarC)
      )
      vals1 <- list()
      vals1[[idCol]] <- 123

      vals2 <- list()
      vals2[[idCol]] <- 456
      vals2[[idColC]] <- "abc"

      client$appeler(
        "tableaux.ajouterÉlément",
        list(idTableau=idTableau, vals=list(vals1, vals2))
      )

      donnéesTableau <- client$obtDonnéesTableau(idTableau = idTableau)

      référence <- tibble::as_tibble(data.frame(colNumérique=c(123, 456), colTexte=c(NA, "abc")))
      names(référence)[names(référence) == "colNumérique"] <- idCol
      names(référence)[names(référence) == "colTexte"] <- idColC

      testthat::expect_equal(donnéesTableau, référence)
    })
})

avecClientEtServeurTest(
  function (client) {
    testthat::test_that("Obtenir données nuée", {
      clefTableau <- "clef"

      idNuée <- client$appeler("nuées.créerNuée")
      idVariable <- client$appeler("variables.créerVariable", list(catégorie="numérique"))
      idTableau <- client$appeler("nuées.ajouterTableauNuée", list(
        idNuée=idNuée,
        clefTableau=clefTableau
      ))
      idCol <- client$appeler('nuées.ajouterColonneTableauNuée', list(
        idTableau=idTableau,
        idVariable=idVariable
      ))
      schéma <- client$appeler("nuées.générerSchémaBdNuée", list(
        idNuée=idNuée, licence="ODbl-1_0"
      ), patience = 60)

      vals1 <- list()
      vals1[[idCol]] <- 123
      vals2 <- list()
      vals2[[idCol]] <- 456

      client$appeler("bds.ajouterÉlémentÀTableauUnique", list(
        schémaBd=schéma,
        idNuéeUnique=idNuée,
        clefTableau=clefTableau,
        vals=list(vals1, vals2)
      ))

      données <- client$obtDonnéesTableauNuée(
        idNuée=idNuée, clefTableau=clefTableau,
        nRésultatsDésirés=100
      )

      idCompte <- client$appeler("obtIdCompte")

      référence <- tibble::tibble(colNumérique=c(123, 456), auteur=c(idCompte, idCompte))
      names(référence)[names(référence) == "colNumérique"] <- idCol

      testthat::expect_equal(
        données, référence
      )
    })
  }
)
