#' Obtenir la version installée du serveur Constellation
#'
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#'
#' @return La version du serveur Constellation
#' @export
#'
#' @examples
#' # Obtenir la version du serveur installé
#' v <- obtVersionServeur()
#' print(v)
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
#'
#' @examples
#' # Obtenir la version de l'IPA installée
#' v <- obtVersionIPAConstellation()
#' print(v)
obtVersionIPAConstellation <- function(exe = "constl") {
  version <- system2(exe, c("v-constl"), stdout = TRUE)
  return(version)
}

#' Installer Constellation sur votre système. Nécessite Node.js (https://nodejs.org/fr) et pnpm (https://pnpm.io/)
#'
#' @export
#' @examples
#' # Installer ou mettre à jour
#' installerConstellation()
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
#' @return Le numéro de port sur lequel le le serveur écoute désormais
#' @export
#'
#' @examples
#' # Lancer sans port défini
#' port <- lancerServeur()
#' # Lancer sur un port prédéfini (il doit être libre)
#' lancerServeur(port=5001)
#' # Spécifier le dossier Orbite ou SFIP
#' lancerServeur(sfip="mon/dossier/SFIP", orbite="mon/dossier/bdOrbite")
lancerServeur <- function(port=NULL, sfip = NULL, orbite = NULL, exe = "constl") {
  # open /Applications/RStudio.app
  # https://community.rstudio.com/t/how-to-get-rstudio-ide-to-use-the-correct-terminal-path-in-mac-os-x/131528/6

  commande <- c("lancer", "-m")

  if (Sys.info()["sysname"] == "Windows") {
    exe <- paste(exe, ".exe", sep="")
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

avecServeur <- function(code, ...) {
  serveur <- lancerServeur(...)
  résultat <- tryCatch(
    {
      code(serveur)
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
