#' la fonction couplage_delta s'applique au cas où le sous_modèle glm_positif est gaussien

#' @param glm_posit : Sous-modele abondance totaux
#' @param glm_pres  : Sous-modele presence/absence
#' @param data_posit: donnees d'abondances (présences seulement)
#' @param data_pres : donnees presence_absence
#' @param tab       : les donnees auxquelles on ajuste le modele
#' @param esp       : espece
#' @param facteur_flotille : facteur pour lequel il est interessant de tracer les series temporelles d'indices modelises. (Ex : Trouver une flotille de tuning)

#' @examples
#'  #PA
#'  delta (tab=tableau_pa, esp="BOBO",list_param, type_donnee="commercial", effort="auto", titre="PA", param_test=list_param, espece_id_list, var_eff_list, ope_id, col_capture, logtrans="auto", interactions="auto", facteur_flotille="engin_peche2", seuil=0.05)
#' @export
#'

delta <- function(tab, esp, list_param, type_donnee, effort, titre, param_test,  espece_id_list, var_eff_list, ope_id, col_capture, logtrans, interactions, facteur_flotille, seuil)
{
  glm_ab <- delta_abondance (tab, esp, param_test, type_donnee, effort, titre, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, logtrans, interactions, seuil)
  glm_pres <- delta_presabs (tab, esp, param_test, type_donnee, effort, titre, list_param,  espece_id_list, var_eff_list, ope_id, col_capture, interactions, seuil)
  ab_pred <- couplage_delta (glm_ab, glm_pres, tab , esp, type_donnee, effort, list_param, espece_id_list, var_eff_list, ope_id, col_capture, facteur_flotille, seuil, titre)
}
