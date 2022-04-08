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
  tab$title <- paste("Observations - Abundance ", title, "\n", facteur)
  ggplot(tab, aes(facteur), color = 'blue') +
    geom_bar() +
    facet_grid(~title)+
    theme_bw() +
    ylab("count") +
    theme(axis.text.x = element_text(angle = 60, size=9),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=9),
          legend.title = element_text(size=10),
          legend.text = element_text(size=10),
          plot.title = element_text(size=10, face="bold"))
}
