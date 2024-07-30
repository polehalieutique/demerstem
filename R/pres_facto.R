#' pres_facto
#'
#' \code{pres_facto} Fonction to present observations for each factor
#'
#' @param tab tableau de données
#' @param facteur facteurs
#' @param titre titre du graphique
#'
#'
#' @export

pres_facto <- function(facteur, tab, titre){
  tab$facteur=factor(tab[,facteur])
  tab$title <- paste("Observations - pres/abs - ", title, "\n", facteur)
  tab$presence <- as.factor(tab$presence)
  ggplot(tab, aes(facteur, fill=presence))+geom_bar() + facet_grid(~title) +
    labs(x = facteur) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, size=9),
          strip.text.x = element_text(face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=9),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10))
}
