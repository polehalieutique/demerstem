#' moda_facto
#'
#' \code{moda_facto} représente les données disponibles par modalite de chaque variable
#'
#' @param tab tableau de données
#' @param facteur facteurs
#' @param title title
#'
#'
#'
#' @export

moda_facto <- function (tab, facteur, title){
  tab$facteur=factor(tab[,facteur])
  tab$title <- paste("Observations - Abundance ", title, "\n", facteur)
  ggplot(tab, aes(facteur)) +
    geom_bar(fill = '#00BFC4') +
    facet_grid(~title)+
    theme_bw() +
    ylab("count") +
    theme(axis.text.x = element_text(angle = 60, size=9),
          strip.text.x = element_text(face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=9),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10))

}
