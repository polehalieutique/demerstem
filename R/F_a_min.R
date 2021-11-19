#'  F_a_min
#'
#' \code{F_a_min} function to be optimized by stats::optimize, hence gives an estimation by iteration of F minimum value
#'
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

F_a_min <- function(x)
{res <- (Mat_Cage[indice]-(Mat_N2[indice]*(x/(x+Mat_M[indice])*(1-exp(-x-Mat_M[indice])))))^2
res}

