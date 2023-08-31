avecServeurTest <- function(code, ...) {
  id <- uuid::UUIDgenerate()
  dossierBase <- file.path(tempdir(), id)

  dossierOrbite <- file.path(dossierBase, "orbite")
  dossierSFIP <- file.path(dossierBase, "sfip")

  params = list(...)
  if (is.null(params$orbite)) params$orbite <- dossierOrbite
  if (is.null(params$sfip)) params$sfip <- dossierSFIP

  # Effacer le dossier temporaire une fois qu'on a fini
  on.exit(unlink(dossierBase, recursive = TRUE), add = TRUE)

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
    function(serveur) {
      expect_equal(class(serveur$port), "numeric")
    }
  )
})

testthat::test_that("lancer serveur port spécifié", {
  avecServeurTest(
    function(serveur) {
      expect_equal(serveur$port, 5123)
    },
    port=5123
  )
})

testthat::test_that("lancer serveur dossier orbite spécifié", {
  dossierOrbite <- file.path(tempdir(), "monDossierOrbite")
  avecServeurTest(
    function (serveur) {
      testthat::expect_true(dir.exists(dossierOrbite))
    },
    orbite = dossierOrbite
  )
})

testthat::test_that("lancer serveur dossier sfip spécifié", {
  dossierSFIP <- file.path(tempdir(), "monDossierSFIP")
  avecServeurTest(
    function (serveur) {
      testthat::expect_true(dir.exists(dossierSFIP))
    },
    sfip = dossierSFIP
  )
})
