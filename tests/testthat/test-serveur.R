créerServeurTest <- function(...) {
  dossierOrbite <- tempdir('orbite')
  dossierSFIP <- tempdir('sfip')

  serveur = constellationR::lancerServeur(..., sfip=dossierSFIP, orbite=dossierOrbite)
  withr::defer(function() {
    serveur$fermer()
    unlink(dossierOrbite)
    unlink(dossierSFIP)
  })
  return(serveur$port)
}

testthat::test_that("obtenir version serveur", {
  version <- obtVersionServeur()
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})

testthat::test_that("obtenir version ipa", {
  version <- obtVersionIPAConstellation()
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})


testthat::test_that("lancer serveur", {
  port <- créerServeurTest()

  expect_equal(class(port), "numeric")
})

testthat::test_that("lancer serveur port spécifié", {
  port <- créerServeurTest(port=5123)
  expect_equal(port, 5123)
})

testthat::test_that("lancer serveur dossier sfip spécifié", {
  dossierSFIP <- tempdir('sfip')
  serveur <- constellationR::lancerServeur(sfip=dossierSFIP)
  serveur$fermer()
  testthat::expect_true(dir.exists(dossierSFIP))
  unlink(dossierSFIP)
})

testthat::test_that("lancer serveur dossier orbite spécifié", {
  dossierOrbite <- tempdir('orbite')
  serveur <- constellationR::lancerServeur(orbite = dossierOrbite)
  serveur$fermer()
  testthat::expect_true(dir.exists(dossierOrbite))
  unlink(dossierOrbite)
})
