#' Cette fonction récapitule la totalite des actions qui concernent le sous-modele abondance y compris la selection des variables

#' @param   tableau           : tableau issu de la préparation des tableaux
#' @param   esp               :  nom de l'espece comme precise dans les donnees
#' @param   param_test        : liste des facteurs potentiels à tester dans le glm
#' @param   type_donnee       : "scientifique" ou "commercial"
#' @param   effort            : "auto" pour un choix automatique de la variable effort ou choix personnel ex "duree_peche",   "nombre_operation","nb_jour_peche", "nb_sorties"
#' @param   formule_select    : permet la selection manuel de la formule à tester dans le glm / "auto" sinon

#' @examples
#'  #PA
#'  delta_abondance(tableau_pa, "BOBO", list_param, "commercial", "auto", titre="PA", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, logtrans="auto", interactions="auto", seuil=0.05)
#' #PI
#' delta_abondance(tableau_pi, "PSEUDOTOLITHUS ELONGATUS", list_param, "commercial", "auto", titre="PI", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil=0.05)
#' #SC
#' delta_abondance(tableau_sc, "PSEUDOTOLITHUS ELONGATUS", list_param, "scientifique", "auto", titre="SC", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil=0.05)
#' @export
delta_abondance <- function(tab, esp, param_test, type_donnee, effort, titre, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions, seuil, formule_select){
  print("SOUS-MODELE ABONDANCE")
  tableau_pres <- indice_ab_pres (tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil)
  tableau_ab <- filter(tableau_pres, presence==1)
  print(param_use(tableau_ab, param_test))
  param <- param_use(tableau_ab, param_test)
  print(lapply(param, moda_facto, tab=as.data.frame(tableau_ab), titre))

  print(evo_an(tableau_ab, titre))
  print(lapply(param, evo_facto, tab=as.data.frame(tableau_ab), titre))

  glm_indice_ab <- glm_ia(tableau_ab, param, interactions, formule_select)

  #Nouvelle partie

  VAR <- var(residuals(glm_indice_ab))
  table_ab <- as.data.frame(coef(summary(glm_indice_ab)))
  table_ab$facteur <- as.factor(rownames(table_ab))
  table_ab$ExpEstimate <- exp(table_ab$Estimate + 0.5*VAR) #correction de Laurent
  table_ab <- table_ab %>% dplyr::select(-"t value")

  for (i in 1:length(param)){
    CODE <- substr(param[i], start = 1, stop = 3)
    table_graph_estim <- table_ab %>% filter(substr(facteur, start = 1, stop = 3) == CODE)
    if (nrow(table_graph_estim) > 0){
      table_graph_estim <- table_graph_estim %>% mutate(variable = as.factor(substr(facteur, start = 1, stop =60)))
      print(ggplot(table_graph_estim) + geom_bar(aes(x=variable, y=ExpEstimate), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(param[i], "pour abondance")) + theme(axis.text.x = element_text(angle = 35)))
    }
  }

  return(glm_indice_ab)
}

