
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
