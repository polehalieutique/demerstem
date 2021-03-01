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

delta_presabs <- function(tab, esp, param_test, type_donnee, effort, titre, list_param,  espece_id_list, var_eff_list, col_capture, seuil, formule_select){
  print("SOUS-MODELE PRESENCE ABSENCE")
  tableau_pres <- indice_ab_pres(tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil)
  print(param_use(tableau_pres, param_test) )
  param <- param_use(tableau_pres, param_test)
  print(lapply(param, pres_facto, tab=tableau_pres, titre))

  #NEW
  for (i in 1:length(param)){
    tableau_pres[,param[i]] <- as.factor(tableau_pres[,param[i]])
    tableau_pres[,param[i]] <- droplevels(tableau_pres[,param[i]])
    contrasts(tableau_pres[,param[i]]) <- contr.sum(levels(tableau_pres[,param[i]]))
  }
  #NEW

  glm_presabs <- glm_pres(tableau_pres, param, formule_select)

  #NEW4

  vect_param <- c(all.vars(formula(glm_presabs))[-1]) # liste des paramètres
  table_finale <- c()
  table_pres <- as.data.frame(coef(summary(glm_pres)))
  for (i in 2:(length(vect_param)+1)){
    table_tempo <- as.data.frame(dummy.coef(glm_presabs)[i])
    table_tempo$modalite <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[i-1]
    colnames(table_tempo) <- c("Estimate", "modalite", "variable")
    table_tempo$ExpEstimate <- table_tempo$Estimate + table_pres[1,1]
    table_tempo$ExpEstimate <- exp(table_tempo$ExpEstimate)/(1+exp(table_tempo$ExpEstimate))
    print(ggplot(table_tempo) + geom_bar(aes(x=modalite, y=ExpEstimate), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[i-1], "pour pres/abs")) + theme(axis.text.x = element_text(angle = 35)))
    table_finale <- rbind(table_finale, table_tempo)
  }

  #NEW4

  return(glm_presabs)
}
