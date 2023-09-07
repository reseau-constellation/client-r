#' Prépare les paramètres d'une fonction Constellation à être envoyés par WebSocket
#'
#' @param paramètres La liste des paramètres
#'
#' @returns Soit les paramètres eux-mêmes, soit, s'ils sont vides, un `data.frame` vide
#'
préparerParamsFoncMessage <- function(paramètres) {
  return(if (is.null(paramètres)) data.frame() else paramètres)
}

#' Prépare le nom d'une fonction Constellation à être envoyé par WebSocket
#'
#' @param nomFonction Le nom de la fonction en format vecteur
#'
#' @returns Le nom formatté en liste si la longeur > 1, sinon le vecteur original
#'
préparerNomFoncMessage <- function(nomFonction) {
  return(if (length(nomFonction) > 1) nomFonction else list(nomFonction))
}

#' Un client Constellation.
#' @description
#' Cette classe se connecte à un serveur Constellation déjà ouvert. Nous vous recommandons de ne pas l'utiliser directement,
#' mais plutôt d'appeler `constellationR::avecClientEtServeur`, ou bien `constellationR::avecClient`, lesquels
#' s'occuperont de la création et de la fermeture du client pour vous.
#'
#' @field port Le numéro du port local sur lequel le serveur est ouvert, et auquel le client se connectera.
#'
#' @export
Client <- R6Class(
  "ClientConstellation",
  private = list(
    écouteurs = list(),
    envoyerMessage = function(m) {
      messageJSON <- jsonlite::toJSON(m, auto_unbox=TRUE, dataframe = "columns")

      private$ws$send(messageJSON)
    },
    ws = NULL
  ),

  public = list(
    initialize = function(
      port
    ) {
      Sys.sleep(2)
      private$ws <- websocket::WebSocket$new(paste("ws://localhost:", as.character(port), sep=""), autoConnect = FALSE)

      ouvert <- FALSE
      private$ws$onOpen(function(event) {
        ouvert <<- TRUE
      })

      private$ws$onMessage(function(event) {
        m <- jsonlite::fromJSON(event$data, simplifyDataFrame = FALSE)
        écouteur <- private$écouteurs[[m$id]]
        if (is.null(écouteur)) {
          return()
        }
        if (m$type == 'action') {
          écouteur$résoudre(m$résultat)
          private$écouteurs[[m$id]] <- NULL
        } else if (m$type == 'suivrePrêt') {
          if (is.null(m$fonctions)){
            écouteur$résoudre(function() {
              private$écouteurs[[m$id]] <- NULL
              messageOublierSuivi <- list(
                type='retour',
                id=m$id,
                fonction="fOublier"
              )
              private$envoyerMessage(messageOublierSuivi)
            })
          } else {
            fonctions <- list()

            for (fonc in m$fonctions) {
              if (fonc == 'fOublier') {
                fonctions[[fonc]] <- function() {
                  private$écouteurs[[m$id]] <- NULL
                  messageOublierSuivi <- list(
                    type='retour',
                    id=m$id,
                    fonction="fOublier"
                  )
                  private$envoyerMessage(messageOublierSuivi)
                }
              } else {
                fonctions[[fonc]] <- function(...) {
                  messageF <- list(
                    type='retour',
                    id=m$id,
                    fonction=fonc,
                    args=list(...)
                  )
                  private$envoyerMessage(messageF)
                }
              }
            }
            écouteur$résoudre(fonctions)
          }
        } else if (m$type == 'suivre') {
          écouteur$f(m$données)
        } else if (m$type == 'erreur') {
          écouteur$rejeter(m$erreur)
          private$écouteurs[[m$id]] <- NULL
        } else {
          stop(paste('Type de message non reconnnu :', m$type, sep=''))
        }
      })
      private$ws$onClose(function(event) {
        cat("Client disconnected with code ", event$code,
            " and reason ", event$reason, "\n", sep = "")
      })
      private$ws$onError(function(event) {
        cat("Client failed to connect: ", event$message, "\n")
      })

      Sys.sleep(1)
      private$ws$connect()
      Sys.sleep(2)

      retry::wait_until(isTRUE(ouvert), timeout = 15)
      Sys.sleep(1)

    },

    action = function(fonction, paramètres = NULL) {
      nomFonction <- résoudreNomFonction(fonction)

      id <- uuid::UUIDgenerate()

      résultat <- NULL
      résultatReçu <- FALSE
      fÉcoute <- function(rés) {
        résultat <<- rés
        résultatReçu <<- TRUE
      }
      fErreur <- function(erreur) {stop(paste("Il y a eu une erreur :", nomFonction, "paramètres", paramètres, erreur))}

      self$enregistrerÉcoute(id, résoudre=fÉcoute, rejeter=fErreur)

      messageAction <- list(
        type='action',
        id=id,
        fonction=préparerNomFoncMessage(nomFonction),
        args=préparerParamsFoncMessage(paramètres)
      )

      private$envoyerMessage(messageAction)

      retry::wait_until(isTRUE(résultatReçu), timeout = 15)

      return(résultat)
    },

    suivre = function(fonction, paramètres = NULL, nomArgFonction='f') {
      nomFonction <- résoudreNomFonction(fonction)
      id <- uuid::UUIDgenerate()

      fOublier <- NULL
      résoudre <- function(f) {
        fOublier <<- f
      }

      fErreur <- function(erreur) {stop(paste("Il y a eu une erreur :", nomFonction, "paramètres", paramètres, erreur))}

      résultatSuivi <- NULL
      appelléAvecFonction <- nomArgFonction %in% names(paramètres)
      if (appelléAvecFonction) {
        f <- paramètres[[nomArgFonction]]
        paramètres[[nomArgFonction]] <- NULL
      } else {
        f <- function(rés) {
          résultatSuivi <<- rés
        }
      }

      self$enregistrerÉcoute(id, résoudre = résoudre, rejeter = fErreur, f = f)

      messageSuivi <- list(
        type="suivre",
        id=id,
        fonction=préparerNomFoncMessage(nomFonction),
        args=préparerParamsFoncMessage(paramètres),
        nomArgFonction=nomArgFonction
      )

      private$envoyerMessage(messageSuivi)
      retry::wait_until(!is.null(fOublier), timeout = 15)

      if (appelléAvecFonction) {
        return(fOublier)
      } else {
        retry::wait_until(!is.null(résultatSuivi), timeout = 15)
        fOublier()
        return(résultatSuivi)
      }

    },

    rechercher = function(fonction, paramètres, nomArgFonction = "f") {
      nomFonction <- résoudreNomFonction(fonction)
      id <- uuid::UUIDgenerate()

      retour <- NULL
      résoudre <- function(f) {
        retour <<- f
      }

      fErreur <- function(erreur) {stop(paste("Il y a eu une erreur :", nomFonction, "paramètres", paramètres, erreur))}

      résultatRecherche <- NULL
      appelléAvecFonction <- nomArgFonction %in% names(paramètres)
      if (appelléAvecFonction) {
        f <- paramètres[[nomArgFonction]]
        paramètres[[nomArgFonction]] <- NULL
      } else {
        f <- function(rés) {
          résultatRecherche <<- rés
        }
      }

      self$enregistrerÉcoute(id, résoudre = résoudre, rejeter = fErreur, f = f)

      messageSuivi <- list(
        type="suivre",
        id=id,
        fonction = préparerNomFoncMessage(nomFonction),
        args = préparerParamsFoncMessage(paramètres),
        nomArgFonction = nomArgFonction
      )

      private$envoyerMessage(messageSuivi)
      retry::wait_until(!is.null(retour), timeout = 15)

      if (appelléAvecFonction) {
        return(retour)
      } else {
        retry::wait_until(!is.null(résultatRecherche), timeout = 15)
        retour$fOublier()
        return(résultatRecherche)
      }

    },

    appeler = function(fonction, paramètres = NULL, nomArgFonction='f') {
      typeFonction <- constellationR::résoudreTypeFonction(fonction)
      if (typeFonction == "action") {
        return(
          self$action(fonction=fonction, paramètres = paramètres)
          )
      } else if (typeFonction == "suivi") {
        return(
          self$suivre(
            fonction=fonction, paramètres = paramètres, nomArgFonction = nomArgFonction
            )
        )
      } else if (typeFonction == "recherche") {
        return(
          self$rechercher(
            fonction = fonction, paramètres = paramètres, nomArgFonction=nomArgFonction
            )
        )
      }
    },

    enregistrerÉcoute = function(id, résoudre, rejeter, f=NULL) {
      private$écouteurs[[id]] <- list(résoudre=résoudre, rejeter=rejeter, f=f)
    },

    fermer = function() {
      private$ws$close()
    }
  ),
)

#' Exécuter du code dans le contexte d'un client Constellation, et fermer le client et le serveur par la suite.
#'
#' @param code Le code à exécuter. Ce code doit être une fonction qui prend le `client` Constellation comme unique paramètre.
#' @param ... Paramètres qui seront passés directement à `constellationR::avecServeur`.
#'
#' @return Le résultat de la fonction `code`.
#' @export

avecClientEtServeur <- function(code, ...) {
  résultat <- avecServeur(
    function(port) {
      résultatClient <- avecClient(
        code,
        port
      )

      return(résultatClient)
    },
    ...
  )

  return(résultat)
}


#' Exécuter du code dans le contexte d'un client Constellation, et fermer le client par la suite. Nécessite qu'un serveur
#' Constellation soit déjà activé sur le `port` spécifié.
#'
#' @param code Le code à exécuter. Ce code doit être une fonction qui prend le `client` Constellation comme unique paramètre.
#' @param port Le port du serveur déjà ouvert.
#'
#' @return Le résultat de la fonction `code`.
#' @export

avecClient <- function(code, port) {
  client <- Client$new(port)
  résultatClient <- tryCatch(
    {
      code(client)
    },
    error = function(cond) {
      message(cond)
      return(cond)
    },
    warning = function(cond) {
      message(cond)
      return(cond)
    },
    finally = {
      client$fermer()
    }
  )

  return(résultatClient)

}
