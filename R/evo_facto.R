#' evolution de l'abondance par parametre (fonction graphique)
# fonction graphique
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
  ggplot(requete, aes(annee, mean_ind_ab)) + geom_point(aes(col=facteur), size=1) + geom_line(aes(group=facteur, color=facteur))+
    theme(axis.text.x = element_text(angle = 60, size=7), plot.title = element_text(size=8, face="bold"), axis.title.x = element_text(size=7), axis.title.y = element_text(size=7), axis.text.y = element_text(size=7)) +
    ggtitle(paste(facteur, titre)) + ylab("mean CPUE") +
    theme(legend.key.size = unit(0.4, "cm"), legend.title = element_text(size=7), legend.text = element_text(size=6))
  #theme(legend.title = element_text(size=8), legend.text = element_text(size=7))

}

