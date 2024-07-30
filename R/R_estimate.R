#'  R_estimate
#'
#' \code{R_estimate} returns Initial recruitment estimate
#'
#' @param   FT            Terminal F
#' @param   Rinit         Initial value for pseudo cohort analysis, equivalent to the last year recruitment
#' @param   Mat_C         data frame of catch, each column is a different gear
#' @param   Mat_R         vector of recruitment, if unknown, considered constant
#' @param   Mat_E         data frame of effort, each column is a different gear
#' @param   Mat_M         vector of mortality biological parameter
#'
#' @export

R_estimate <- function(FT, age, Mat_C, Mat_R, Mat_E, Mat_M) {

  R_pseudo_rectif_bis <-function(init) # init <- R_init
  {
    Mat_F2<<-Mat_C/Mat_Cage # attention, initialisation de F2 au ratio de chaque metier
    indice<<-1
    Mat_N2[indice]<<-init
    Mat_F2[indice,]<<-Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
    # réallocation du F estimé pour chaque métier
    Mat_q[indice,]<<-Mat_F2[indice,]/Mat_E[1,]
    Mat_N2[indice+1]<<-F_abundance(indice)

    for(compteur in 2:(age))
    {
      indice<<-compteur
      Mat_F2[indice,]<<-Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
      Mat_q[indice,]<<-Mat_F2[indice,]/Mat_E[1,]
      if (indice<age) Mat_N2[indice+1]<<-F_abundance(indice)
    }
    #on renvoie la somme
    if (nb_flot < 2)
    {
      Mat_F2[age,] # si 1 seul engin
    }
    else
    {
      apply(Mat_F2[age,],1,sum) # si multi-engins
    }
  }


  F_pseudo_rectif_global<-function(init)
  {
    res<-(FT-R_pseudo_rectif_bis(init))^2
    res
  }

  FT_init <- 0.2
  VPA_Pope(FT, age, Mat_Cage, Mat_M)
  Rinit <- Mat_N[1]

  MAT_N<<-rep(NA,age) # effectifs methode de Pope
  Mat_N2<<-rep(NA,age) # effectifs methode rectifiee
  Mat_C <<- Mat_C
  nb_flot<<- ncol(Mat_C)# nombre de flottilles
  Mat_Cage <<- apply(Mat_C,1,sum) # somme des captures de chaque flottille
  Mat_C <<- Mat_C
  Mat_E <<- Mat_E
  Mat_R <<- Mat_R
  Mat_M <<- Mat_M
  Mat_q <<- Mat_C #initialisation des capturabilites par les captures
  indice <<- 1 # age de recrutement
  optimize(F_pseudo_rectif_global,interval=c(Rinit/10,Rinit*10))$minimum

  # plot(Mat_Cage, xlab = "Age", ylab = "Catch (numbers)", type = "b", main= paste0("Catch at age - ", espece))
  # print("Mat_F2")
  # print(Mat_F2)
  # Mat_F<- apply(Mat_F2,1,sum)
  # plot(Mat_F, xlab = "Age", ylab = "Fishing mortality (numbers)", type = "b", main= paste0("F at age - ", espece))
  # print("Mat_N2")
  # print(Mat_N2)
  # plot(Mat_N2, xlab = "Age", ylab = "Abundance (numbers)", type = "b", main= paste0("Abundance at age - ", espece))
}
