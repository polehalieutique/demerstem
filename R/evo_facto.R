#' evolution de l'abondance par parametre (fonction graphique)
#' \code{evo_facto} fonction graphique
#' @param tab tableau de données
#' @param facteur facteur
#' @param titre titre du graphique

#' @examples
#' lapply(param_pa, evo_facto, tab=tableau_pa_ab, "PA")
#' lapply(param_pi, evo_facto, tab=tableau_pi_ab, "PI")
#' lapply(param_sc, evo_facto, tab=tableau_sc_ab, "SC")

#' @export

evo_facto <- function(tab, facteur, titre){
  #tab$annee=as.numeric(as.character(tab$annee))
  tab$annee=factor(tab$annee)
  tab$facteur=factor(tab[,facteur])

  requete <- tab %>% dplyr::group_by(facteur, annee) %>% dplyr::summarise(mean_ind_ab=mean(i_ab))
  requete$title <- paste("CPUE -", title, "\n", facteur)
    ggplot(requete, aes(annee, mean_ind_ab)) +
      geom_point(aes(col=facteur), size=1.2) +
      geom_line(aes(group=facteur, color=facteur), size = 1.1) +
    facet_grid(~title)+
    theme_bw() +
    ylab("mean CPUE") +
    theme(axis.text.x = element_text(angle = 60, size=9),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=9),
          legend.title = element_text(size=8),
          legend.text = element_text(size=8),
          plot.title = element_text(size=10, face="bold"))
    #theme(legend.key.size = unit(0.4, "cm"), legend.title = element_text(size=7), legend.text = element_text(size=6))
}

