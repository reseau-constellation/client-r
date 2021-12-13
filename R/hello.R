# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# https://www.r-bloggers.com/2020/07/how-to-write-your-own-r-package-and-publish-it-on-cran/

# https://rstudio.github.io/promises/articles/overview.html

# https://combine-australia.github.io/r-pkg-dev/documenting-functions.html

#' Lancer un serveur Constellation local
#'
#' @param port Le numéro du port sur lequel le port sera connecté
#' @param autoinstaller Si on devrait installer Constellation pour vous, si ce n'est pas déjà fait
#' @param exe La commande pour lancer Constellation. Uniquement nécessaire pour une installation de Constellation non standard
#'
#' @return Le numéro de port sur lequel le le serveur écoute désormais
#' @export
#'
#' @examples
#' # Lancer sans port défini
#' port <- lancerServeur()
#' # Lancer sur un port prédéfini (il doit être libre)
#' lancerServeur(port=5001)
lancerServeur <- function(port=NULL, autoinstaller=TRUE, exe = "constl") {

}

lancerClient <- function() {
  ws <- websocket::WebSocket$new("ws://localhost:5002")
  ws$onOpen(function(event) {
    cat("Connection opened\n")
  })
  ws$onMessage(function(event) {
    cat("Client got msg: ", event$data, "\n")
  })
  ws$onClose(function(event) {
    cat("Client disconnected with code ", event$code,
        " and reason ", event$reason, "\n", sep = "")
  })
  ws$onError(function(event) {
    cat("Client failed to connect: ", event$message, "\n")
  })
  ws$connect()
}

