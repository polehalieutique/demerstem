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
#' data(length_frequency)
#' lmax <- 48 # choose lmax according to some bibliography
#' K <- seq(0.33, 0.40, 0.01)
#' L_inf <- seq(0.95*lmax, 1.05 * lmax, 0.01)
#' step_class <- 1
#' method_shepherd(data_freq, K, L_inf, step_class,  plot=F)
#' @export

method_shepherd <- function (data_freq, K, L_inf, step_class, plot = FALSE)
{
  print("Shepherd method for growth parameters estimation")
  combi <- expand.grid(K = K, L_inf = L_inf, S = 0)
  data_freq$month <- data_freq$month/12
  if (plot == T) {
    S_final <- 0
    for (i in 1:nrow(combi)) {
      K <- combi[i, 1]
      L <- combi[i, 2]
      S <- 0
      for (j in 1:nrow(data_freq)) {
        if ((data_freq$lclass[j] + step_class) < L) {
          t_min <- (1/K) * log(L/(L - data_freq$lclass[j]))
          t_max <- (1/K) * log(L/(L - (data_freq$lclass[j] +
                                         step_class)))
          T_1 <- (sin(pi * (t_max - t_min))/(pi * (t_max -
                                                     t_min))) * cos(2 * pi * (((t_max + t_min)/2) -
                                                                                data_freq$month[j]))
          T_final <- T_1 * sqrt(data_freq$total_sampling[j])
          S <- S + T_final
        }
        combi[i, 3] <- S
      }
      if (S > S_final) {
        S_final <- S
        K_final <- K
        L_final <- L
      }
    }
    plot(ggplot(combi, aes(x = L_inf, y = K_inf, col = S)) +
           geom_point())
  }
  else {
    S_final <- 0
    for (i in 1:nrow(combi)) {
      K <- combi[i, 1]
      L <- combi[i, 2]
      S <- 0
      for (j in 1:nrow(data_freq)) {
        if ((data_freq$lclass[j] + step_class) < L) {
          t_min <- (1/K) * log(L/(L - data_freq$lclass[j]))
          t_max <- (1/K) * log(L/(L - (data_freq$lclass[j] +
                                         step_class)))
          T_1 <- (sin(pi * (t_max - t_min))/(pi * (t_max -
                                                     t_min))) * cos(2 * pi * (((t_max + t_min)/2) -
                                                                                data_freq$month[j]))
          T_final <- T_1 * sqrt(data_freq$total_sampling[j])
          S <- S + T_final
        }
      }
      if (S > S_final) {
        S_final <- S
        K_final <- K
        L_final <- L
      }
    }
  }
  D <- c(S_final, K_final, L_final)
  names(D) <- c("S_final", "K_final", "L_final")
  return(D)
}
