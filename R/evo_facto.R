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
  tab$annee=factor(tab$annee)
  tab$facteur=factor(tab[,facteur])
  requete <- tab %>% group_by(facteur, annee) %>% summarise(mean_ind_ab=mean(i_ab))
  ggplot(requete, aes(annee, mean_ind_ab)) + geom_point(aes(col=facteur)) + geom_line(aes(group=facteur, color=facteur))+
    theme(axis.text.x = element_text(angle = 60)) +
    ggtitle(paste(facteur, titre))
}
