testthat::test_that("Résolution nom fonction", {
  résoluPoint <- résoudreNomFonction("profil.ajouterNoms")
  testthat::expect_equal(résoluPoint, c("profil", "ajouterNoms"))

  résoluListe <- résoudreNomFonction(c("profil", "ajouterNoms"))
  testthat::expect_equal(résoluListe, c("profil", "ajouterNoms"))
})

testthat::test_that("Auto-détection type action", {
  résolu <- résoudreTypeFonction("obtIdCompte")
  testthat::expect_equal(résolu, "action")
})

testthat::test_that("Auto-détection type suivi", {
  résolu <- résoudreTypeFonction("profil.suivreCourriel")
  testthat::expect_equal(résolu, "suivi")
})

testthat::test_that("Auto-détection type recherche", {
  résolu <- résoudreTypeFonction("rechercher.rechercherBdsSelonNom")
  testthat::expect_equal(résolu, "recherche")
})

testthat::test_that("Conversion données tableau - une valeur vide", {
  données <- jsonlite::fromJSON(
    "{\"données\":[{\"col1\":123},{\"col2\":\"abc\",\"col1\":456}]}",
    simplifyDataFrame = FALSE
  )
  td <- donnéesTableauÀTrame(données["données"])
  testthat::expect_equal(td, tibble::tibble(col1=c(123, 456), col2=c(NA, "abc")))
})

testthat::test_that("Conversion données tableau - données vides", {
  données <- jsonlite::fromJSON(
    "{\"données\":[]}",
    simplifyDataFrame = FALSE
  )
  td <- donnéesTableauÀTrame(données["données"])
  testthat::expect_equal(td, tibble::tibble())
})

testthat::test_that("Conversion données nuées - toutes valeurs présentes", {
  auteur <- "/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH"
  données <- jsonlite::fromJSON(
    "{\"données\":[{\"col1\":123,\"auteur\":\"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"},{\"col1\":456,\"auteur\":\"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"}]}",
    simplifyDataFrame = FALSE
  )
  td <- donnéesTableauÀTrame(données["données"])
  testthat::expect_equal(td, tibble(col1=c(123, 456), auteur=c(auteur, auteur)))
})

testthat::test_that("Conversion données nuées - une valeur vide", {
  auteur <- "/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH"
  données <- jsonlite::fromJSON(
    "{\"données\":[{\"col1\":123,\"auteur\":\"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"},{\"col1\":456,\"col2\":\"abc\",\"auteur\":\"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"}]}",
    simplifyDataFrame = FALSE
  )
  td <- donnéesTableauÀTrame(données["données"])
  testthat::expect_equal(td, tibble::tibble(col1=c(123, 456), auteur=c(auteur, auteur), col2=c(NA, "abc")))
})

testthat::test_that("Conversion données nuée - données vides", {
  données <- jsonlite::fromJSON(
    "{\"données\":[]}",
    simplifyDataFrame = FALSE
  )
  td <- donnéesTableauÀTrame(données["données"])
  testthat::expect_equal(td, tibble::tibble())
})
