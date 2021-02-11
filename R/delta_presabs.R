#' la fonction delta_presabs récapitule les actions qui concernent le sous-modele presence-absence y compris la selection des variables

#' @param tableau = tableau issu de la préparation des tableaux
#' @param esp = nom de l'espece comme precise dans les donnees
#' @param param_test = liste des facteurs potentiels à tester dans le glm
#' @param formule_select = permet la selection manuel de la formule à tester dans le glm / "auto" sinon

#' @examples
#'  #PA
#'  delta_presabs(tableau_pa, "BOBO", list_param, "commercial", "auto", titre="PA", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions = "auto", seuil=0.05)
#'  #PI
#'  delta_presabs(tableau_pi, "PSEUDOTOLITHUS ELONGATUS", list_param, "commercial", "auto", titre="PI", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions = "auto", seuil=0.05)
#'  #SC
#'  delta_presabs(tableau_sc, "PSEUDOTOLITHUS ELONGATUS", list_param, "scientifique", "auto", titre="SC", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions = "auto", seuil=0.05)
#' @export
#'

delta_presabs <- function(tab, esp, param_test, type_donnee, effort, titre, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions, seuil, formule_select){
  tableau_pres <- indice_ab_pres (tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil)
  print(param_use(tableau_pres, param_test) )
  param <- param_use(tableau_pres, param_test)
  print("SOUS-MODELE PRESENCE ABSENCE")
  print(lapply(param, pres_facto, tab=tableau_pres, titre))
  glm_presabs <- glm_pres(tableau_pres, param, interactions, formule_select)

  #Nouvelle partie
  table_pres <- as.data.frame(coef(summary(glm_presabs)))
  table_pres$facteur <- as.factor(rownames(table_pres))
  table_pres$ExpEstimate <- exp(table_pres$Estimate)/(1+exp(table_pres$Estimate))
  table_pres<- table_pres %>% dplyr::select(-"z value")
  table_pres$facteur <- as.character(table_pres$facteur)
  i<-2
  for (i in 1:length(param)){
    CODE <- substr(param[i], start = 1, stop = 3)
    table_graph_estim <- table_pres %>% filter(substr(facteur, start = 1, stop = 3) == CODE)
    if (nrow(table_graph_estim) > 0){
      table_graph_estim <- table_graph_estim %>% mutate(variable = as.factor(substr(facteur, start = 1, stop =60)))
      print(ggplot(table_graph_estim) + geom_bar(aes(x=variable, y=ExpEstimate), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(param[i], "pour pres/abs")) + theme(axis.text.x = element_text(angle = 35)))
    }
  }

  return(glm_presabs)
}
