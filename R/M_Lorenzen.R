#' Natural mortality estimate
#'
#' \code{M_Lorenzen} Estimate natural mortality accordingly to Lorenzen methods (1996, 2000)
#'
#' @param list_age  list of age for which we want to estimate M
#' @param M_inf Asymptotic natural mortality
#' @param K Growth parameter
#' @param t0 Growth parameter
#'
#' @examples
#' list_age <- seq(0,3, by = 0.25)
#' M_inf <- 0.2
#' K <- 0.35
#' t0 <- -0.24
#'
#' res <- M_Lorenzen(list_age, M_inf, K, t0)
#'
#' res
#' @export
#'
M_Lorenzen <- function(list_age, M_inf, K, t0){
  plot <- list()
  M <- M_inf*(1-exp(-K*(list_age-t0)))^-(3.08*0.21) #M_u * (a * (L_inf*(1-exp(-K*(list_age-t0))))^b)^d
  data <- data.frame(M= M, age = list_age)
  plot[[1]] <- ggplot(data,aes(x = age, y = M)) + geom_point() + geom_line() + theme_nice() + ylim(c(0,max(M)))
  return(list(M, plot))
}
