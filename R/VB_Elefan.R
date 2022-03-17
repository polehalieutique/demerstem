#'  VB_Elefan
#'
#' \code{VB_Elefan} estimates R_init
#'
#' @param   data_freq          data with weight and size vectors
#' @param   L_inf
#' @param   step_class
#' @param   MA
#' @param   season
#' @examples
#'  data(data_weight_size)
#'  model_weight(data_weight_size)
#'
#' @export

VB_Elefan <- function(data_freq, Linf_guess, step_class, MA = 5, season = F) {
  seasonalised = season
  set.seed(1) # set seed value for reproducible results
  if(is.null(data_freq$year)) {
    df <- data_freq %>% mutate(date = gsub(" ", "", paste("01",".",month,".","2000")))
  }
  else {
    df <- data_freq %>% mutate(date = gsub(" ", "", paste("01",".",month,".",year)))
  }

  df$date <- as.Date(df$date, format = "%d.%m.%Y")
  lfq <- lfqCreate(data = df, Lname = "lclass", Dname = "date", Fname = "total_sampling")
  lfq_modif <- lfqModify(lfq, bin_size = step_class)

  lfq_visu <- lfqRestructure(lfq_modif, MA = MA, addl.sqrt = FALSE)

  plot(lfq_visu, Fname = "catch", date.axis = "modern")
  plot(lfq_visu, Fname = "rcounts", date.axis = "modern")

  low_par <- list(Linf = 0.8 * Linf_guess,
                  K = 0.01,
                  t_anchor = 0,
                  C = 0,
                  ts = 0)


  up_par <- list(Linf = 1.2 * Linf_guess,
                 K = 1,
                 t_anchor = 1,
                 C = 1,
                 ts = 1)

  res_SA <- ELEFAN_SA(lfq_modif, SA_time = 70*0.5, SA_temp = 6e5,
                      MA = MA, seasonalised, addl.sqrt = FALSE,
                      init_par = list(Linf = Linf_guess,
                                      K = 0.5,
                                      t_anchor = 0.5,
                                      C=0.5,
                                      ts = 0.5),
                      low_par = low_par,
                      up_par = up_par)

  plot(res_SA)
  print(res_SA$par)
  print(res_SA$Rn_max)

  res_GA <- ELEFAN_GA(lfq_modif, MA = MA, seasonalised = season,
                      maxiter = 50, addl.sqrt = FALSE,
                      low_par = low_par,
                      up_par = up_par,
                      monitor = FALSE)
  plot(res_GA)
  print(res_GA$par)
  print(res_GA$Rn_max)

}
