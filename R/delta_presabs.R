#' la fonction delta_presabs récapitule les actions qui concernent le sous-modele presence-absence y compris la selection des variables

#' @param tableau = tableau issu de la préparation des tableaux
#' @param esp = nom de l'espece comme precise dans les donnees
#' @param param_test = liste des facteurs potentiels à tester dans le glm

#' @examples
#'  #PA
#'  delta_presabs(tableau_pa, "BOBO", list_param, "commercial", "auto", titre="PA", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions = "auto", seuil=0.05)
#'  #PI
#'  delta_presabs(tableau_pi, "PSEUDOTOLITHUS ELONGATUS", list_param, "commercial", "auto", titre="PI", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions = "auto", seuil=0.05)
#'  #SC
#'  delta_presabs(tableau_sc, "PSEUDOTOLITHUS ELONGATUS", list_param, "scientifique", "auto", titre="SC", list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions = "auto", seuil=0.05)
#' @export
#'
delta_presabs <- function(tab, esp, param_test, type_donnee, effort, titre, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions, seuil){
  tableau_pres <- indice_ab_pres (tab, type_donnee, effort, esp, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, seuil)
  print(param_use(tableau_pres, param_test) )
  param <- param_use(tableau_pres, param_test)
  print("SOUS-MODELE PRESENCE ABSENCE")
  print(lapply(param, moda_facto, tab=tableau_pres, titre))
  print(lapply(param, pres_facto, tab=tableau_pres, titre))
  glm_presabs <- glm_pres(tableau_pres, param, interactions)
  return(glm_presabs)
}
