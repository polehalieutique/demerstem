#'  Shepherd method
#'
#' \code{method_shepherd} returns an estimation of growth parameters K and L_inf (growth coefficient and growth asymptotic length) from Von Bertallanfy equation and the value of objective function.
#'
#' @param   data_freq       input dataset data_freqle
#' @param   title           fraction of the title in the plots
#' @param   K               range aof tested values for K
#' @param   L_inf           range of tested valyes for L_inf
#' @param   step_class      length step between two length class
#' @param   plot            FALSE by default. IF TRUE, print plot of S

#'
#' @examples
#'  data(data_freqleau_sc)
#'  model_ai_plus(data_freqleau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

method_shepherd <- function(data_freq, K, L_inf, step_class,  plot=FALSE){
  print("Shepherd method for growth parameters estimation")
  combi <- expand.grid(K_inf=K_inf, L_inf=L_inf, S = 0)
  data_freq$month <- data_freq$month/12 # define seasonality
  if(plot==T){
  S_final <- 0
  for (i in 1:nrow(combi)){
    K <- combi[i, 1]
    L <- combi[i, 2]
    S <- 0
    for (j in 1:nrow(data_freq)){
      if((data_freq$lclass[j] + step_class) < L){
        t_min <- (1 / K) * log(L / (L - data_freq$lclass[j]))
        t_max <- (1 / K) * log(L / (L - (data_freq$lclass[j] + step_class)))
        T_1 <- (sin(pi * (t_max - t_min)) / (pi * (t_max - t_min))) * cos(2 * pi * (((t_max + t_min) / 2) - data_freq$month[j]))
        T_final <- T_1 * sqrt(data_freq$total_sampling[j])
        S <- S + T_final
      }
      combi[i,3] <- S
     }
    if (S>S_final){
      S_final <- S
      K_final <- K
      L_final <- L
     }
  }
  plot(ggplot(combi, aes(x=L_inf, y=K_inf, col=S)) + geom_point())
  }

  else{
    S_final <- 0
  for (i in 1:nrow(combi)){
    K <- combi[i, 1]
    L <- combi[i, 2]
    S <- 0
    for (j in 1:nrow(data_freq)){
      if((data_freq$lclass[j] + step_class) < L){
        t_min <- (1 / K) * log(L / (L - data_freq$lclass[j]))
        t_max <- (1 / K) * log(L / (L - (data_freq$lclass[j] + step_class)))
        T_1 <- (sin(pi * (t_max - t_min)) / (pi * (t_max - t_min))) * cos(2 * pi * (((t_max + t_min) / 2) - data_freq$month[j]))
        T_final <- T_1 * sqrt(data_freq$total_sampling[j])
        S <- S + T_final
      }
    }
    if (S>S_final){
      S_final <- S
      K_final <- K
      L_final <- L
    }
  }
  }
  D  <- c(S_final, K_final, L_final)
  names(D) <- c("S_final", "K_final", "L_final")
  # list2env(D, envir = .GlobalEnv)
  return(D)

}
