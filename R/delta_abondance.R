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
  tableau_pres <- indice_ab_pres(tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil)
  tableau_ab <- filter(tableau_pres, presence==1)
  print(param_use(tableau_ab, param_test))
  param <- param_use(tableau_ab, param_test)
  print(lapply(param, moda_facto, tab=as.data.frame(tableau_ab), titre))

  print(evo_an(tableau_ab, titre))
  print(lapply(param, evo_facto, tab=as.data.frame(tableau_ab), titre))

  #NEW
  for (i in 1:length(param)){
    tableau_ab[,param[i]] <- as.factor(tableau_ab[,param[i]])
    tableau_ab[,param[i]] <- droplevels(tableau_ab[,param[i]])
    contrasts(tableau_ab[,param[i]]) <- contr.sum(levels(tableau_ab[,param[i]]))
  }
  #NEW

  glm_indice_ab <- glm_ia(tableau_ab, param, formule_select)

  #NEW4
  VAR <- var(residuals(glm_indice_ab))
  vect_param <- c(all.vars(formula(glm_indice_ab))[-1]) # liste des paramètres
  table_finale <- c()
  table_ab <- as.data.frame(coef(summary(glm_ab)))
  for (i in 2:(length(vect_param)+1)){
    table_tempo <- as.data.frame(dummy.coef(glm_indice_ab)[i])
    table_tempo$modalite <- rownames(table_tempo)
    rownames(table_tempo) <- NULL
    table_tempo$variable <- vect_param[i-1]
    colnames(table_tempo) <- c("Estimate", "modalite", "variable")
    table_tempo$ExpEstimate <- table_tempo$Estimate + table_ab[1,1]
    table_tempo$ExpEstimate <- exp(table_tempo$Estimate + 0.5*VAR)
    print(ggplot(table_tempo) + geom_bar(aes(x=modalite, y=ExpEstimate), stat="identity", color = "black", fill = "white") + ylab("Estimateur") + ggtitle(paste(vect_param[i-1], "pour abondance")) + theme(axis.text.x = element_text(angle = 35)))
    table_finale <- rbind(table_finale, table_tempo)
  }

  #NEW4

  return(glm_indice_ab)
}

