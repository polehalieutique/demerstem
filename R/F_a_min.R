#'  F_a_min
#'
#' \code{F_a_min} function to be optimized by stats::optimize, hence gives an estimation by iteration of F minimum value
#'
#'
#' @examples
#' Mat_Cage <- apply(Mat_C, 1, sum) # catch sum by age over fleets
#' Mat_M <- rep(0.2, age)
#' age <- length(Mat_M) # number of age class
#' Mat_N2 <- rep(NA,age) # initial abundance
#' nbflot <- length(Mat_C) # number of gear
#' assign('nbflot', nbflot, envir=globalenv())
#' Mat_q <- Mat_C #initialisation des capturabilites par les captures
#' indice <- 1 # age de recrutement
#
#' # furst calcul of F, q and N from R, C, E and M.
#' Mat_F2 <- Mat_C/Mat_Cage #  initialisation of F with a ration by gear
#' indice <- 1
#' assign('indice', 1, envir=globalenv())
#' Mat_N2[indice] <- Rinit
#' assign('Mat_N2', Mat_N2, envir=globalenv())
#' Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min, interval=c(0,2))$minimum
#' @export

F_a_min <- function(x)
{res <- (Mat_Cage[indice]-(Mat_N2[indice]*(x/(x+Mat_M[indice])*(1-exp(-x-Mat_M[indice])))))^2
res}

