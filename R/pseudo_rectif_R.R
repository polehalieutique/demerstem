#'  pseudo_rectif_R
#'
#' \code{pseudo_rectif_R} returns abundance, capturability and fishing mortality of a class age (F and N) - readapted from Chassot et al. 2008
#'
#'
#' @param   Rinit          Initial value for pseudo cohort analysis, equivalent to the last year recruitment
#' @param   Mat_C         data frame of catch, each column is a different gear
#' @param   Mat_R         vector of recruitment, if unknown, considered constant
#' @param   Mat_E         data frame of effort, each column is a different gear
#' @param   Mat_M         vector of mortality biological parameter

#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

pseudo_rectif_R <- function(Mat_Cage, Mat_C, Mat_R, Mat_E, Mat_M, Rinit)
{
  print("Rectified pseudo-cohort analysis")
  # parameters initialisation
  age <- length(Mat_M) # number of age class
  Mat_N2 <- rep(NA,age) # initial abundance
  nbflot <- length(Mat_C) # number of gear
  assign('nbflot', nbflot, envir=globalenv())
  Mat_q <- Mat_C #initialisation des capturabilites par les captures
  indice <- 1 # age de recrutement

  # furst calcul of F, q and N from R, C, E and M.
  Mat_F2 <- Mat_C/Mat_Cage #  initialisation of F with a ration by gear
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
