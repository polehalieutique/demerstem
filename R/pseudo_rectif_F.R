#'  pseudo_rectif_F
#'
#' \code{pseudo_rectif_F} rectified VPA from a initial value of F Terminal
#'
#' @param   init          Initial value for pseudo cohort analysis, equivalent to the F Terminal
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

pseudo_rectif_F <-function(Rinit, FT, Mat_Cage, Mat_C, Mat_R, Mat_E, Mat_M)
{
  res<-( FT - pseudo_rectif_R(Rinit, Mat_Cage, Mat_C, Mat_R, Mat_E, Mat_M) )^2
  res
}
