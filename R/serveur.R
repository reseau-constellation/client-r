#' Obtenir la version installée du serveur Constellation
#'
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#'
#' @return La version du serveur Constellation
#' @export
#'

obtVersionServeur <- function(exe = "constl") {
  # Ou peut-être https://www.r-bloggers.com/2021/09/how-to-use-system-commands-in-your-r-script-or-package/ ?
  version <- system2(exe, c("version"), stdout = TRUE)
  return(version)
}

#' Installer Constellation sur votre système. Nécessite Node.js (https://nodejs.org/fr) et pnpm (https://pnpm.io/)
#'
#' @return Rien
#' @export

installerConstellation <- function() {
  system2("curl", c("https://raw.githubusercontent.com/reseau-constellation/serveur-ws/principale/installer.cjs", "|", "node", "-"), stdout = TRUE)
}

#' Lancer un serveur Constellation local
#'
#' @param port Le numéro du port sur lequel le port sera connecté
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#' @param dossier Le dossier du compte Constellation à utiliser (optionnel)
#'
#' @return Le numéro de port sur lequel le le serveur écoute désormais, et une fonction à appeler pour fermer le serveur
#' @export
#'

lancerServeur <- function(port=NULL, dossier = NULL, exe = "constl") {
  # open /Applications/RStudio.app
  # https://community.rstudio.com/t/how-to-get-rstudio-ide-to-use-the-correct-terminal-path-in-mac-os-x/131528/6

  commande <- c("lancer", "-m")

  if (Sys.info()["sysname"] == "Windows") {
    exe <- paste(exe, ".CMD", sep="")
  }

  if (!is.null(port)) {
    commande <- c(commande, "--port", port)
  }
  if (!is.null(dossier)) {
    commande <- c(commande, "--dossier", dossier)
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

      if (grepl("MESSAGE MACHINE", ligne)) {
        messageMachine <- jsonlite::fromJSON(
          stringi::stri_split_fixed(ligne, ":", 2)[[1]][2]
        )

        if (messageMachine$type == "NŒUD PRÊT") {
          portFinal <- as.numeric(messageMachine$port)
          codeSecret <- messageMachine$codeSecret
          break
        }
      }
    }
  }

  if (is.null(portFinal) || is.null(codeSecret)) {
    stop("Serveur mal initialisé.")
  }

  fermer <- function() {
    p$write_input("\n", sep="\n")
    p$wait(2)
    p$kill()
  }

  return(list(port=portFinal, codeSecret=codeSecret, fermer=fermer))
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
      code(serveur$port, serveur$codeSecret)
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
