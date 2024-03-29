% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{résoudreNomFonction}
\alias{résoudreNomFonction}
\title{Résout le nom de la fonction en séparant ses composantes.}
\usage{
résoudreNomFonction(nomFonction)
}
\arguments{
\item{nomFonction}{Le nom de la fonction; par exemple, "profil.sauvegarderNoms"}
}
\value{
Le nom décomposé de la fonction en format vecteur
}
\description{
Résout le nom de la fonction en séparant ses composantes.
}
\examples{
nomRésolu <- résoudreNomFonction("profil.sauvegarderNoms")
nomRésolu == c("profil", "sauvegarderNoms")

}
