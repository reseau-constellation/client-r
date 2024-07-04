#' Prépare les paramètres d'une fonction Constellation à être envoyés par WebSocket
#'
#' @param paramètres La liste des paramètres
#'
#' @returns Soit les paramètres eux-mêmes, soit, s'ils sont vides, un `data.frame` vide
#'
préparerParamsFoncMessage <- function(paramètres) {
  if (is.null(paramètres)) {
    return(data.frame())
  }
  # Important d'effacer les paramètres NULL, qui seront sinon formatés en `{}` dans json
  if (length(paramètres)) {
    paramètres[sapply(paramètres, is.null)] <- NULL
  }

  return(paramètres)
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
    #'
    #' @param port Le numéro du port local sur lequel le serveur est ouvert, et auquel le client se connectera.
    #' @param codeSecret Le code secret pour pouvoir se connecter au serveur.
    #'
    initialize = function(
      port,
      codeSecret=NULL
    ) {
      Sys.sleep(2)

      if (is.null(codeSecret)) {
        requèteCode <- httr2::request(paste("http://localhost/demande?id=Client R ", as.character(sample(1000:9999, 1)[1]), as.character(port), sep=""))
        réponse <- httr2::req_perform(requèteCode)
        codeSecret <- httr2::resp_body_string(réponse)
      }
      urlWs <- paste("ws://localhost:", as.character(port), "?code=", utils::URLencode(codeSecret), sep="")
      print(urlWs)
      private$ws <- websocket::WebSocket$new(urlWs, autoConnect = FALSE)

      ouvert <- FALSE
      private$ws$onOpen(function(event) {
        print("ws ouverte")
        ouvert <<- TRUE
      })

      private$ws$onMessage(function(event) {
        print(event$data)
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
      print("on va connecter")
      private$ws$connect()
      print("on est connecté")
      Sys.sleep(2)

      retry::wait_until(isTRUE(ouvert), timeout = 30)
      Sys.sleep(1)

    },

    #' Fonction pour invoquer un action sur Constellation.
    #'
    #' @param fonction Le nom de la fonction à invoquer (p. ex., "variables.créerVariable")
    #' @param paramètres Liste nommée avec les paramètres de la fonction
    #' @param patience Le nombre de secondes qu'on va attendre pour une réponse
    #' avant de perdre patience.
    #'
    #' @return Le résultat ded la fonction invoquée.
    #'
    action = function(fonction, paramètres = NULL, patience = 15) {
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

      retry::wait_until(isTRUE(résultatReçu), timeout = patience)

      return(résultat)
    },

    #' Fonction pour invoquer un suivi sur Constellation.
    #'
    #' @param fonction Le nom de la fonction à invoquer (p. ex., "profil.suivreNoms")
    #' @param paramètres Liste nommée avec les paramètres de la fonction
    #' @param nomArgFonction Le nom du paramètre correspondant à la
    #' fonction de suivi (voir documentation IPA Constellation).
    #' "f" par défaut.
    #' @param condition Condition nécessaire pour valider le premier résultat
    #' à retourner. Uniquement utilisé si `paramètres[[nomArgFonction]]` n'existe pas.
    #' @param patience Le nombre de secondes qu'on va attendre pour une réponse
    #' avant de perdre patience.
    #'
    #' @return Si `paramètres[[nomArgFonction]]` existe, cette fonction sera invoqué de manière
    #' continue chaque fois que les résultats changent, et la fonction
    #' `suivre` elle-même retournera une fonction pour annuler le suivi.
    #' Si `paramètres[[nomArgFonction]]` n'existe pas, retourne le premier résultat obtenu.
    #'
    suivre = function(fonction, paramètres = NULL, nomArgFonction='f', condition=function(x) !is.null(x), patience = 15) {
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
      retry::wait_until(!is.null(fOublier), timeout = patience)

      if (appelléAvecFonction) {
        return(fOublier)
      } else {
        retry::wait_until(condition(résultatSuivi), timeout = patience)
        fOublier()
        return(résultatSuivi)
      }

    },

    #' Fonction pour invoquer une recherche sur Constellation.
    #'
    #' @param fonction Le nom de la fonction à invoquer (p. ex., "recherche.rechercherVariablesSelonNom")
    #' @param paramètres Liste nommée avec les paramètres de la fonction
    #' @param nomArgFonction Le nom du paramètre correspondant à la fonction
    #' de suivi (voir documentation IPA Constellation).
    #' "f" par défaut.
    #' @param patience Le nombre de secondes qu'on va attendre pour une réponse
    #' avant de perdre patience.
    #'
    #' @return Si `paramètres[[nomArgFonction]]` existe, cette fonction sera invoqué de manière
    #' continue chaque fois que les résultats de la recherche changent, et la fonction
    #' `recherche` elle-même retournera des fonctions pour annuler la recherche et pour
    #' changer le nombre de résultats désirés. Si `paramètres[[nomArgFonction]]` n'existe
    #' pas, retourne le premier résultat obtenu par la recherche.
    #'
    rechercher = function(fonction, paramètres, nomArgFonction = "f", patience = 15) {
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
      retry::wait_until(!is.null(retour), timeout = patience)

      if (appelléAvecFonction) {
        return(retour)
      } else {
        retry::wait_until(!is.null(résultatRecherche), timeout = patience)
        retour$fOublier()
        return(résultatRecherche)
      }

    },

    #' Fonction pour invoquer une fonction (action, recherche, ou suivi)
    #' de Constellation.
    #'
    #' @param fonction Le nom de la fonction à invoquer (p. ex., "bds.créerBd")
    #' @param paramètres Liste nommée avec les paramètres de la fonction, si approprié
    #' @param nomArgFonction S'il s'agit d'un fonction de suivi ou de recherche, le nom du paramètre
    #' correspondant à la fonction de suivi (voir documentation IPA Constellation).
    #' "f" par défaut.
    #' @param patience Le nombre de secondes qu'on va attendre pour une réponse
    #' avant de perdre patience.
    #'
    #' @return Le résultat de la fonction
    #'
    appeler = function(fonction, paramètres = NULL, nomArgFonction='f', patience = 45) {
      typeFonction <- constellationR::résoudreTypeFonction(fonction)
      if (typeFonction == "action") {
        return(
          self$action(fonction=fonction, paramètres = paramètres, patience = patience)
          )
      } else if (typeFonction == "suivi") {
        return(
          self$suivre(
            fonction=fonction, paramètres = paramètres, nomArgFonction = nomArgFonction, patience = patience
            )
        )
      } else if (typeFonction == "recherche") {
        return(
          self$rechercher(
            fonction = fonction, paramètres = paramètres, nomArgFonction=nomArgFonction, patience = patience
            )
        )
      }
    },

    #' Méthode privée. Touche pas.
    #'
    #' @param id Identifiant unique
    #' @param résoudre Fonction résolution
    #' @param rejeter Fonction rejet
    #' @param f Fonction de suivi
    #'
    enregistrerÉcoute = function(id, résoudre, rejeter, f=NULL) {
      private$écouteurs[[id]] <- list(résoudre=résoudre, rejeter=rejeter, f=f)
    },

    #' Fonction rapide pour obtenir des données d'un tableau en format tibble
    #'
    #' @param idTableau L'identifiant du tableau
    #' @param langues Liste optionnelle des langues (en ordre de préférence) dans lesquelles on veut obtenir les résultats
    #'
    #' @return Les données en format tibble::tibble
    #'
    obtDonnéesTableau = function(idTableau, langues=NULL) {
      données <- self$suivre(
        "tableaux.suivreDonnéesExportation",
        paramètres = list(idTableau=idTableau, langues=langues)
      )
      td <- donnéesTableauÀTrame(données["données"])

      return(td)
    },

    #' Fonction rapide pour obtenir des données d'une nuée en format tibble
    #'
    #' @param idNuée L'identifiant de la nuée
    #' @param clefTableau La clef du tableau d'intérêt
    #' @param langues Liste optionnelle des langues (en ordre de préférence) dans lesquelles on veut obtenir les résultats
    #' @param nRésultatsDésirés Le nombre de résultats désirés
    #'
    #' @return Les données en format tibble::tibble
    #'
    obtDonnéesTableauNuée = function(
      idNuée, clefTableau, nRésultatsDésirés=100, langues=NULL
    ) {
      données <- self$suivre(
        "nuées.suivreDonnéesExportationTableau",
        paramètres = list(
          idNuée=idNuée,
          clefTableau=clefTableau,
          nRésultatsDésirés=nRésultatsDésirés,
          langues=langues
        )
      )

      return(donnéesNuéeÀTrame(données["données"]))
    },

    #' Fermer le client
    #' @return Rien
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
    function(port, codeSecret) {
      résultatClient <- avecClient(
        code,
        port,
        codeSecret
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
#' @param codeSecret Le code secret pour se connecter au serveur.
#'
#' @return Le résultat de la fonction `code`.
#' @export

avecClient <- function(code, port, codeSecret) {
  client <- Client$new(port, codeSecret)
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
