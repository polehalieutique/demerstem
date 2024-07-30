#'  evolution de l'indice d'abondance moyen dans le temps
#' \code{evo_an} fonction graphique
#' @param tab tableau de données
#' @param titre titre du graphique

#' @examples
#' PA #
#'evo_an(tableau_pa_ab, "PA")
#' evo_an(tableau_pi_ab, "PI")
#' evo_an(tableau_sc_ab, "SC")
#' @export
evo_an<- function(tab, titre){
  tab<-data.frame(tab)
  tab$annee=as.factor(tab$annee)
  requete <- tab %>% dplyr::group_by(annee) %>% dplyr::summarise(mean_ind_ab=mean(i_ab))

  brp <- ggplot(requete, aes(annee, mean_ind_ab))+ geom_bar(stat="identity", fill="darkblue") + ggtitle(paste("evolution indice abondance", titre)) + theme(axis.text.x = element_text(angle = 60))
  bxp <- ggplot(tab, aes(annee, i_ab)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 60))
  ggarrange(brp, bxp, ncol = 2, nrow = 1)
}
