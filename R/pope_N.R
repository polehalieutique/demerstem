#'  pope_abundance
#'
#' \code{pope_abundance} estimates N
#'
#' @param   age    from VPA_pope
#'
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

pope_N <- function(indice)
{
  Mat_N[indice] <- Mat_N[indice+1]*exp(Mat_M[indice])+Mat_Cage[indice]*exp(Mat_M[indice]/2)
  assign('Mat_N', Mat_N, envir=globalenv())
}
