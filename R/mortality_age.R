#'  M_lorenzen
#'
#' \code{M_lorenzen} function to be optimized by stats::optimize, hence gives an estimation by iteration of F minimum value
#'
#'
#' @examples
#' list_age <- seq(1,3.75, by = 0.25)
#' M_inf <- 0.2
#' b <- 3.15
#' t0 <- -0.23
#' K <- 0.2
#' espece <- "T. est"
#' age_max = 5
#' title = paste0("Mortality at age - Lorenzen method - \n", espece)
#' graph_param <- c(age_max, title)
#' M_lorenzen(M_inf, b, t0, K, list_age, graph_param)
#' @export

M_lorenzen <-  function(list_age, M_inf, K, t0){

  M <- M_inf*(1-exp(-K*(list_age-t0)))^c #M_u * (a * (L_inf*(1-exp(-K*(list_age-t0))))^b)^d

  data <- data.frame(M= M, age = list_age)

  plot(ggplot(data,aes(x = age, y = M)) + geom_point() + geom_line() + theme_nice() + ylim(c(0,max(M))))

  return(M)
}

