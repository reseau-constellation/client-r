#' Obtenir la version installée du serveur Constellation
#'
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#'
#' @return La version du serveur Constellation
#' @export


obtVersionServeur <- function(exe = "constl") {
  # Ou peut-être https://www.r-bloggers.com/2021/09/how-to-use-system-commands-in-your-r-script-or-package/ ?
  version <- system2(exe, c("version"), stdout = TRUE)
  return(version)
}

#' Obtenir la version installée de l'IPA Constellation
#'
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#'
#' @return La version de l'IPA Constellation
#' @export


obtVersionIPAConstellation <- function(exe = "constl") {
  version <- system2(exe, c("v-constl"), stdout = TRUE)
  return(version)
}

#' Installer Constellation sur votre système. Nécessite Node.js (https://nodejs.org/fr) et pnpm (https://pnpm.io/)
#'
#' @return Rien
#' @export

installerConstellation <- function() {
  system2("pnpm", c("i", "--global", "@constl/ipa", "@constl/serveur"), stdout = TRUE)
}

#' Lancer un serveur Constellation local
#'
#' @param port Le numéro du port sur lequel le port sera connecté
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#' @param sfip Le dossier SFIP à utiliser (optionnel)
#' @param orbite Le dossier du compte Constellation à utiliser (optionnel)
#'
#' @return Le numéro de port sur lequel le le serveur écoute désormais, et une fonction à appeler pour fermer le serveur
#' @export
#'


lancerServeur <- function(port=NULL, sfip = NULL, orbite = NULL, exe = "constl") {
  # open /Applications/RStudio.app
  # https://community.rstudio.com/t/how-to-get-rstudio-ide-to-use-the-correct-terminal-path-in-mac-os-x/131528/6

  commande <- c("lancer", "-m")

  if (Sys.info()["sysname"] == "Windows") {
    exe <- paste(exe, ".CMD", sep="")
    print(system2("where.exe", c("constl")))
  }

  if (!is.null(port)) {
    commande <- c(commande, "--port", port)
  }
  if (!is.null(sfip)) {
    commande <- c(commande, "--doss-sfip", sfip)
  }
  if (!is.null(orbite)) {
    commande <- c(commande, "--doss-orbite", orbite)
  }

  p <- processx::process$new(exe, commande, stdout = "|", stdin = "|", encoding = "UTF-8")

  portFinal <- NULL
  tempsDébut <- Sys.time()
  while (difftime(Sys.time(), tempsDébut, units="secs") <= 30) {
    p$poll_io(5000)

    résultat <- p$read_output_lines()
    if (!length(résultat)) {break}

    for (l in length(résultat)) {
      ligne <- résultat[l]
      print(ligne)

      if (grepl("MESSAGE MACHINE", ligne)) {
        messageMachine <- jsonlite::fromJSON(
          stringi::stri_split_fixed(ligne, ":", 2)[[1]][2]
        )

        if (messageMachine$type == "NŒUD PRÊT") {
          portFinal <- as.numeric(messageMachine$port)
          break
        }
      }
    }
  }

  if (is.null(portFinal)) {
    stop("Serveur mal initialisé.")
  }
  fermer <- function() {
    p$write_input("\n", sep = "\n")
    p$wait(2)
    p$kill()
  }

  return(list(port=portFinal, fermer=fermer))
}

#' Exécuter du code dans le contexte d'un serveur Constellation, et fermer le serveur par la suite.
#'
#' @param code Le code à exécuter. Ce code doit être une fonction qui prend le `port` du serveur comme unique paramètre.
#' @param ... Arguments qui seront passés directement à `constellationR::lancerServeur`.
#'
#' @return Le résultat de la fonction `code`.
#' @export

avecServeur <- function(code, ...) {
  serveur <- lancerServeur(...)
  résultat <- tryCatch(
    {
      code(serveur$port)
    },
    error = function(cond) {
      message(cond)
      print(cond)
      return(cond)
    },
    warning = function(cond) {
      message(cond)
      print(cond)
      return(cond)
    },
    finally = {
      serveur$fermer()
    }
  )
  return(résultat)
}
