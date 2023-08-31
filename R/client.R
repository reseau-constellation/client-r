Client <- R6Class(
  "ClientConstellation",
  private = list(
    écouteurs = list(),
    envoyerMessage = function(m) {
      messageJSON <- jsonlite::toJSON(m, method="C")
      print("message à envoyer")
      print(messageJSON)
      private$ws$send(messageJSON)
      print("message envoyé")
    },
    ws = NULL
  ),

  public = list(
    initialize = function(
      port
    ) {
      print("ici 0")
      ws <- websocket::WebSocket$new(paste("ws://localhost:", as.character(port), sep=""), autoConnect = FALSE)
      print("ici 1")
      ws$onOpen(function(event) {
        print("C'est ouvert !")
      })
      print("ici 2")
      ws$connect()
      print("ici 3")
      ws$send("hello")

      private$ws$onMessage(function(event) {
        print("Message reçu : ", event$data, "\n")

        m <- jsonlite::fromJSON(event$data)
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
                id=id,
                fonction="fOublier"
              )
              private$envoyerMessage(messageOublierSuivi)
            })
          } else {
            fonctions = list()
            for (fonc in names(m$fonctions)) {
              if (fonc == 'fOublier') {
                fonctions[[fonc]] = function() {
                  private$écouteurs[[m$id]] <- NULL
                  messageOublierSuivi <- list(
                    type='retour',
                    id=id,
                    fonction="fOublier"
                  )
                  private$envoyerMessage(messageOublierSuivi)
                }
              } else {
                fonctions[[fonc]] = function(...args) {
                  private$envoyerMessage(list(
                    type='retour',
                    id=m$id,
                    fonction=fonc,
                    args: args
                  ))
                }
              }
            }
            écouteur$résoudre(fonctions)
          }
        } else if (m$type == 'suivre') {
          écouteur$f(m$résultat)
        } else if (m$type == 'erreur') {
          écouteur$rejeter(m$erreur)
          private$écouteurs[[m$id]] <- NULL
        } else {
          stop(paste('Type de message non reconnnu :', m$type, sep=''))
        }
      })
      print("ici 4")
      private$ws$onClose(function(event) {
        cat("Client disconnected with code ", event$code,
            " and reason ", event$reason, "\n", sep = "")
      })
      print("ici 5")
      private$ws$onError(function(event) {
        cat("Client failed to connect: ", event$message, "\n")
      })
      print("ici 6")

    },

    action = function(fonction, paramètres = NULL) {
      nomFonction <- résoudreNomFonction(fonction)

      id <- uuid::UUIDgenerate()

      résultat <- NULL
      fÉcoute <- function(rés) {
        résultat <<- rés
      }
      fErreur <- stop(paste("Il y a eu une erreur :", nomFonction, "paramètres", paramètres))

      self$enregistrerÉcoute(id, résoudre=fÉcoute, rejeter=fErreur)

      messageAction <- list(
        type='action',
        id=id,
        fonction=nomFonction,
        args=if (is.null(paramètres)) list() else paramètres
      )

      private$envoyerMessage(messageAction)

      retry::wait_until(!is.null(réssultat))

      return(résultat)
    },

    suivre = function(fonction, paramètres, nomArgFonction='f') {
      nomFonction <- résoudreNomFonction(fonction)

      id <- uuid::UUIDgenerate()

      écoutePrète <- FALSE
      résoudre <- function() {
        écoutePrète <<- TRUE
      }

      résultatSuivi <- NULL
      appelléAvecFonction <- nomArgFonction %in% names(paramètres)
      if (appelléAvecFonction) {
        f <- paramètres[[nomArgFonction]]
      } else {
        f <- function(rés) {
          résultatSuivi <<- rés
        }
      }

      self$enregistrerÉcoute(id, résoudre = résoudre, rejeter = rejeter, f = f)

      message <- list(
        type="suivre",
        id=id,
        fonction=nomFonction,
        args=if (is.null(paramètres)) list() else paramètres,
        nomArgFonction=nomArgFonction,
      )

      private$envoyerMessage(messageSuivi)
      retry::wait_until(isTRUE(écoutePrète))

      messageOublier <- list(
        type="retour",
        id=id,
        fonction="fOublier",
      )
      fOublier <- function() {
        private$envoyerMessage(messageOublier)
      }

      if (appelléAvecFonction) {
        return(fOublier)
      } else {
        retry::wait_until(!is.null(résultatSuivi))
        fOublier()
        return(résultatSuivi)
      }

    },

    rechercher = function( ) {

    },

    enregistrerÉcoute = function(id, résoudre, rejeter, f=NULL) {
      private$écouteurs[[id]] <- list(résoudre=résoudre, rejeter=rejeter, f=f)
    },

    fermer = function() {
      private$ws$disconnect()
    }
  ),
)

#'
#' @export
avecClient <- function(code, ...) {
  résultat <- avecServeur(
    function(serveur) {
      client <- Client$new(serveur$port)
      return(code(client))
    },
    ...
  )

  return(résultat)
}
