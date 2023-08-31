
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

résoudreNomFonction <- function(nomFonction) {
  if (is.vector(nomFonction) & length(nomFonction) > 1) {
    return(nomFonction)
  } else {
    return(strsplit(nomFonction, split="[.]")[[1]])
  }
}
