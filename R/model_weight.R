#'  model_weight
#'
#' \code{model_weight} estimates R_init
#'
#' @param   data_weight          data with weight and size vectors
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

model_weight <- function(data_weight) {
  M1 <- nls(formula = pt ~ a*(lt ^ b),
            start = list(a= 0.05, b=2.5), algorithm = "default", trace = T,data=data_weight)
  plot(data_weight$pt ~ data_weight$lt, xlim = c(0,max(data_weight$lt)), ylim = c(0,max(data_weight$pt), xlab = "Length (cm)", ylab = "Weight (g)")
  curve(weight_size(x, p=c( a = coef(M1)[1], b = coef(M1)[2])), from = 0, to = 1000, n = 100, col = 'red', lwd = 2, add= TRUE)
  a <- coef(M1)[1]
  b <- coef(M1)[2]
  print(paste0("a = ", round(a,4)," b = ",round(b,4)))
}
