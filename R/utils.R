
#' Essaie de deviner de quel type est une fonction Constellation.
#'
#' @param nomFonction Le nom de la fonction selon la documentation Constellation (https://docu.réseau-constellation.ca), par exemple, "obtIdCompte" ou "variables.créerVariable"
#'
#' @returns Le type de fonction, soit "suivi", "action", ou "recherche".
#'
#' @export
#'
#' @examples
#' typeFonction <- résoudreTypeFonction("recherche.rechercherVariablesSelonNom")
#' typeFonction == "recherche"
#'
résoudreTypeFonction <- function(nomFonction) {
  nomRésolu <- résoudreNomFonction(nomFonction)
  nomFonction <- nomRésolu[length(nomRésolu)]
  if (startsWith(nomFonction, "suivre")) {
    return("suivi")
  } else if (startsWith(nomFonction, "rechercher")) {
    return("recherche")
  }
  return("action")
}

#' Résout le nom de la fonction en séparant ses composantes.
#'
#' @param nomFonction Le nom de la fonction; par exemple, "profil.sauvegarderNoms"
#'
#' @returns Le nom décomposé de la fonction en format vecteur
#'
#' @export
#'
#' @examples
#' nomRésolu <- résoudreNomFonction("profil.sauvegarderNoms")
#' nomRésolu == c("profil", "sauvegarderNoms")
#'
résoudreNomFonction <- function(nomFonction) {
  if (is.vector(nomFonction) & length(nomFonction) > 1) {
    return(nomFonction)
  } else {
    return(strsplit(nomFonction, split="[.]")[[1]])
  }
}

#' Transforme des données de tableau de format Constellation en
#' format de trame de données tibble R.
#'
#' @param données Les données provenant de Constellation
#'
#' @return Une trame de données en format tibble R
#'
#' @examples
#' données <- jsonlite::fromJSON("{
#'   \"données\":[{
#'     \"col1\":123},{\"col2\":\"abc\",\"col1\":456}
#'    ]}"
#'   , simplifyDataFrame = FALSE
#'  )
#' td <- donnéesTableauÀTrame(données["données"])
#'
donnéesTableauÀTrame <- function(données) {
  colonnes <- unique(unlist(as.vector(sapply(données[[1]], function (x) names(x)))))
  nRangées <- length(données[[1]])

  trame_données <- data.frame(matrix(nrow=nRangées, ncol=length(colonnes)))
  colnames(trame_données) <- colonnes
  for (colonne in colonnes) {
    trame_données[[colonne]] <- sapply(données[[1]], function (x) if (is.null(x[[colonne]])) NA else x[[colonne]])
  }
  return(as_tibble(trame_données))
}

#' Transforme des données de nuée de format Constellation en
#' format de trame de données tibble R.
#'
#' @param données Les données provenant de Constellation
#'
#' @return Une trame de données en format tibble R
#'
#' @examples
#' données <- jsonlite::fromJSON(
#'   "{\"données\":[
#'      {\"col1\":123,
#'      \"auteur\": \"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"},
#'      {\"col2\":\"abc\",\"col1\":456,
#'      \"auteur\": \"/orbitdb/zdpuB1wjvzSEsY9YZ4Z2kUEX2DLzwV9G8LCQnzfLccHgY1LdH\"
#'      }]}",
#'   simplifyDataFrame = FALSE
#'  )
#' td <- donnéesTableauÀTrame(données["données"])
#'
donnéesNuéeÀTrame <- function(données) {
  return(donnéesTableauÀTrame(données))
}
