#'  Présentation des facteurs
#' @param tab tableau de données
#' @param facteur facteurs
#' @param titre titre du graphique

#' @examples
#' PA #
#' tableau_pa_pres <- indice_ab_pres (tableau_pa, "commercial", "auto", esp="BOBO", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil=0.005)
#' lapply(param_pa, pres_facto, tab=tableau_pa_ab, "PA") # peche artisanale


#' @export

pres_facto <- function(facteur, tab, titre){
  tab$facteur=factor(tab[,facteur])
  ggplot(tab, aes(facteur, fill=factor(presence)))+geom_bar() +ggtitle(paste("presence/absence", facteur, titre)) +
    theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
}
