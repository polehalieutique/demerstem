#'  R_pseudo_rectif
#'
#' \code{R_pseudo_rectif} returns abundance, capturability and fishing mortality of a class age (F and N) from initial recruitment - readapted from Chassot et al. 2008
#'
#'
#' @param   Rinit         Initial value for pseudo cohort analysis, equivalent to the last year recruitment
#' @param   Mat_C         data frame of catch, each column is a different gear
#' @param   Mat_R         vector of recruitment, if unknown, considered constant
#' @param   Mat_E         data frame of effort, each column is a different gear
#' @param   Mat_M         vector of mortality biological parameter

#' @examples
#' #exemple tiré de Bertignac, 1987
#' espece <- "Dicentrarchus labrax"
#' age <- 18
#' Mat_C <- data.frame(catchA = c(8, 438, 13955, 16933, 47864, 22925,23117,4684,15853,10942,8839,18524,8141,9850,4485,3608,2642,1310), catchB = c(0, 184, 1827, 2245, 7158, 4792,7151,1767,5378,3524,1429,2533,1481,2470,1161,563,617,477), catchC = c(8, 1103, 11285, 5938, 9773, 2749,1722,228,678,483,368,213,87,265,224,87,85,0), catchD = c(7, 6435, 20817, 4023, 3656, 905,570,117,131,60,115,42,111,157,117,0,51,0))
#' Mat_Cage <- apply(Mat_C, 1, sum) # catch sum by age over fleets
#' Mat_M <- rep(0.2, age)
#' Mat_E <- data.frame(effortA = c(3500, 2900, 2200, 1500, 800, 800,800,800,800,800,800,800,800,800,800,800,800,800), effortB = c(800, 600, 450, 350, 200, 50,50,0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0), efforC = c(550, 400, 200, 100, 100, 100,100,100,100,100,100,100,100,100,100,100,100,100), effortD = c(430, 600, 760, 900, 900, 1000,1100,1150,1250,1350,1350,1350,1350,1350,1350,1350,1350,1350))
#' Mat_R <- rep(100000, 18) # si indisponible, on suppose recrutement constant
#' Mat_M <- rep(0.2, 18)
#' Rinit <- 1.18*10^6#  Mat_N[1] # valeur issue de Bertignac, 1987
#' pseudo_rectif_R(Mat_Cage, Mat_C, Mat_R, Mat_E, Mat_M, Rinit)
#' @export

R_pseudo_rectif <- function(Rinit, Mat_C, Mat_R, Mat_E, Mat_M)
{
  print("Rectified pseudo-cohort analysis")
  # parameters initialisation
  age <- length(Mat_M) # number of age class
  Mat_N2 <- rep(NA,age) # initial abundance
  nbflot <- length(Mat_C) # number of gear
  assign('nbflot', nbflot, envir=globalenv())
  Mat_q <- Mat_C #initialisation des capturabilites par les captures
  indice <- 1 # age de recrutement
  Mat_Cage <- apply(Mat_C, 1, sum)
  # furst calcul of F, q and N from R, C, E and M.
  Mat_F2 <- Mat_C/Mat_Cage #  initialisation of F with a ratio by gear
  indice <- 1
  assign('indice', 1, envir=globalenv())
  Mat_N2[indice] <- Rinit
  assign('Mat_N2', Mat_N2, envir=globalenv())
  Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min, interval=c(0,2))$minimum
  assign('Mat_F2', Mat_F2, envir=globalenv())
  print(Mat_F2[indice,])
  Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
  assign('Mat_q', Mat_q, envir=globalenv())
  Mat_N2[indice+1] <- F_abundance(indice)
  assign('Mat_N2', Mat_N2, envir=globalenv())

  for(compteur in 2:(age)){
    indice <- compteur
    assign('indice', indice, envir=globalenv())

    Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min, interval=c(0,2))$minimum
    assign('Mat_F2', Mat_F2, envir=globalenv())
    Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
    assign('Mat_q', Mat_q, envir=globalenv())

    if (indice<age) {
      Mat_N2[indice+1] <- F_abundance(indice)
      assign('Mat_N2', Mat_N2, envir=globalenv())
    }

    if (nbflot<2) {
      Mat_F2[age,] # si 1 seul engin
    }
    else
    {
      apply(Mat_F2[age,], 1, sum) # si multi-engins
    }
  }

  plot(Mat_Cage, xlab = "Age", ylab = "Catch (numbers)", type = "b", main= paste0("Catch at age - ", espece))
  print("Mat_F2")
  print(Mat_F2)
  Mat_F<- apply(Mat_F2,1,sum)
  plot(Mat_F, xlab = "Age", ylab = "Fishing mortality (numbers)", type = "b", main= paste0("F at age - ", espece))
  print("Mat_N2")
  print(Mat_N2)
  plot(Mat_N2, xlab = "Age", ylab = "Abundance (numbers)", type = "b", main= paste0("Abundance at age - ", espece))

}
