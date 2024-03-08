avecServeurTest <- function(code, ...) {
  id <- uuid::UUIDgenerate()
  dossier <- file.path(tempdir(), id)

  params <- list(...)
  if (is.null(params$dossier)) params$dossier <- dossier

  # Effacer le dossier temporaire une fois qu'on a fini
  on.exit(unlink(dossier, recursive = TRUE), add = TRUE)

  résultat <- do.call(
    constellationR::avecServeur,
    c(
      code,
      params
    )
  )
  return(résultat)
}

testthat::test_that("obtenir version serveur", {
  version <- obtVersionServeur()
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})

testthat::test_that("obtenir version ipa", {
  version <- constellationR::obtVersionIPAConstellation()
  expect_equal(length(strsplit(version, "\\.")[[1]]), 3)
})


testthat::test_that("lancer serveur", {
  avecServeurTest(
    function(port) {
      expect_equal(class(port), "numeric")
    }
  )
})

testthat::test_that("lancer serveur port spécifié", {
  avecServeurTest(
    function(port) {
      expect_equal(port, 5123)
    },
    port=5123
  )
})

testthat::test_that("lancer serveur dossier Constellation spécifié", {
  dossier <- file.path(tempdir(), "monDossierConstellation")
  avecServeurTest(
    function (port) {
      testthat::expect_true(dir.exists(dossier))
    },
    dossier = dossier
  )
})

