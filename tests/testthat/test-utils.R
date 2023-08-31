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
