#'  F_abundance
#'
#' \code{F_abundance} returns abundance of a class age
#'
#'
#' @param   indice          class age
#'

#' @examples
#' #' Mat_Cage <- apply(Mat_C, 1, sum) # catch sum by age over fleets
#' Mat_M <- rep(0.2, age)
#' age <- length(Mat_M) # number of age class
#' Mat_N2 <- rep(NA,age) # initial abundance
#' nbflot <- length(Mat_C) # number of gear
#' assign('nbflot', nbflot, envir=globalenv())
#' Mat_q <- Mat_C #initialisation des capturabilites par les captures
#' indice <- 1 # age de recrutement
#'
#' # furst calcul of F, q and N from R, C, E and M.
#' Mat_F2 <- Mat_C/Mat_Cage #  initialisation of F with a ration by gear
#' indice <- 1
#' assign('indice', 1, envir=globalenv())
#' Mat_N2[indice] <- Rinit
#' assign('Mat_N2', Mat_N2, envir=globalenv())
#' Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min, interval=c(0,2))$minimum
#' assign('Mat_F2', Mat_F2, envir=globalenv())
#' print(Mat_F2[indice,])
#' Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
#' assign('Mat_q', Mat_q, envir=globalenv())
#' Mat_N2[indice+1] <- F_abundance(indice)
#' assign('Mat_N2', Mat_N2, envir=globalenv())
#'
#' for(compteur in 2:(age)){
#'   indice <- compteur
#'   assign('indice', indice, envir=globalenv())
#'
#'   Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min, interval=c(0,2))$minimum
#'   assign('Mat_F2', Mat_F2, envir=globalenv())
#'   Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
#'   assign('Mat_q', Mat_q, envir=globalenv())
#'
#'   if (indice<age) {
#'     Mat_N2[indice+1] <- F_abundance(indice)
#'     assign('Mat_N2', Mat_N2, envir=globalenv())
#'   }
#'   if (nbflot<2) {
#'     Mat_F2[age,] # si 1 seul engin
#'   }
#'   else
#'   {
#'     apply(Mat_F2[age,], 1, sum) # si multi-engins
#'   }
#' }
#' @export

F_abundance <- function(indice){
  res <- Mat_N2[1]*(Mat_R[indice+1]/Mat_R[1]) #calcul du R qui conduit à l'effectif age 1
  i <- 1
  for (j in indice :1){
    if (nbflot<2){
      res<-res*exp(-(Mat_q[i,]*Mat_E[j+1,])-Mat_M[j]) # N pour un seul métier
    }else{
      res<-res*exp(-apply((Mat_q[i,]*Mat_E[j+1,]),1,sum)-Mat_M[j])  #  N pour pluri-métier
    }
    i<-i+1
  }
  res
}
