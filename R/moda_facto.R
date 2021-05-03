#'  nombre de donnees par modalite de chaque parametre
#' @param tab tableau de données
#' @param facteur facteurs
#' @param titre titre du graphique

#' @examples
#' PA #
#' tableau_pa_pres <- indice_ab_pres (tableau_pa, "commercial", "auto", esp="BOBO", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil=0.005)
#' lapply(param_pa, moda_facto, tab=tableau_pa_ab, "PA") # peche artisanale
#' lapply(param_pi, moda_facto, tab=tableau_pi_ab, "PI") # peche industrielle
#' lapply(param_sc, moda_facto, tab=tableau_sc_ab, "SC") # campagnes scientifiques


#' @export

moda_facto <- function (tab, facteur, titre){
  tab$facteur=factor(tab[,facteur])
  ggplot(tab, aes(facteur))+ geom_bar() + ggtitle(paste(facteur, titre)) + ylab("nombre de donnees") + theme(axis.text.x = element_text(angle = 45)) +
    theme(axis.text.x = element_text(angle = 60, size=8), plot.title = element_text(size=10, face="bold"), axis.title.x = element_text(size=8), axis.title.y = element_text(size=8), legend.title = element_text(size=8), legend.text = element_text(size=8))
}
