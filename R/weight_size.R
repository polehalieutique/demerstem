#'  weight_size
#'
#' \code{weight_size} to plot size-weight relationship
#'
#' @param   t          time vector
#' @param   p          vector with values
#'
#' @examples
#'  t <- 1
#'  p <- c(0.005,3.16)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

weight_size <- function(t,p)
{
  res <- p[1]*t^p[2]
}
