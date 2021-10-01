#' Delta coupling of 2 GLMs
#'
#' \code{delta_glm} realize the delta-coupling using the 2 precedents GLMs outputs (filtered with year modality only), extract the year factor, calculate the AI and display the plots.
#'
#'
#' @param glm_pres_abs    : table of effects values for each modality of the year factor taken from model_pres_abs (recalculated if needed)
#' @param glm_abundance   : table of effects values for each modality of the year factor taken from model_ai_plus (recalculated if needed)
#' @param title           : first part of the title for the plots
#' @param type            : type of the data series. Ex : "SC", "PA", etc...
#'
#' @examples
#' data(tableau_sc)
#' glm_abundance <- model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"),espece_id='nom_taxonomique', catch_col='total_capture', interactions ="N", limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#' glm_pres_abs <- model_pres_abs(tableau_sc, esp="PSEUDOTOLITHUS ELONGATUS", effort="auto", title="SC", list_param=c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions = "N", limit=0.0001, formula_select = "presence ~ strate + annee + saison")
#' delta_glm(glm_pres_abs, glm_abundance, title="Scientific campaign GIN BOBO", type = "SC")
#' @export


delta_glm<-function(glm_pres_abs,glm_abundance, title, type){

  table_finale1 <- glm_pres_abs %>% dplyr::select(modality, corrected_estimates)
  table_finale2 <- glm_abundance %>% dplyr::select(modality, corrected_estimates)

  table_finale1$modality <- as.character(table_finale1$modality)
  table_finale2$modality <- as.character(table_finale2$modality)
  # Couplage
  table_annee_final <- inner_join(table_finale1, table_finale2, by='modality') #Inner Join? Ou Left joint? Avec left on a des NA.
  table_annee_final <- table_annee_final %>% mutate(EstimateurFinal = corrected_estimates.x * corrected_estimates.y) %>% dplyr::select(modality, EstimateurFinal)
  #table_annee_final <- table_annee_final %>% mutate(annee = as.factor(substr(facteur, start = 6, stop = 9)))
  colnames(table_annee_final) <- c("annee", paste0("EstimateurFinal_",type))

  #g1<-ggplot(table_annee_final) + geom_bar(aes(x=annee, y=EstimateurFinal), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(title, "avec années en facteur")) + theme(axis.text.x = element_text(angle = 35)) #"IA pêche scientifique Guinée"


  #Plot avec Annee as numeric
  table_annee_final$annee <- as.numeric(as.character(table_annee_final$annee))

  EstimateurFinal <- as.name(paste0("EstimateurFinal_",type))

  g2 <- ggplot(table_annee_final) + geom_bar(aes_string(x='annee', y= paste0("EstimateurFinal_",type)), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(title, "avec annees en numérique"))
  g3 <- ggplot(table_annee_final) + geom_line(aes_string(x='annee', y= paste0("EstimateurFinal_",type)), stat="identity") + ylab("Indice d'Abondance") + ggtitle(paste(title, "avec annees en numérique")) + scale_y_continuous(limits=c(0, max(table_annee_final$EstimateurFinal))) #+ ylim(0,max(table_annee_final$EstimateurFinal)))
  #print(g1)
  print(g2)
  print(g3)

  return (table_annee_final)
}

