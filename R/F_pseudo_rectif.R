#'  F_pseudo_rectif
#'
#' \code{F_pseudo_rectif} returns Initial recruitment estiMate
#'
#' @param   FT            Terminal F
#' @param   Rinit         Initial value for pseudo cohort analysis, equivalent to the last year recruitment
#' @param   Mat_C         data frame of catch, each column is a different gear
#' @param   Mat_R         vector of recruitment, if unknown, considered constant
#' @param   Mat_E         data frame of effort, each column is a different gear
#' @param   Mat_M         vector of mortality biological parameter
#'
#' @export

F_pseudo_rectif <- function(FT, age, Mat_C, Mat_R, Mat_E, Mat_M) {

  R_pseudo_rectif_bis <-function(init) # init <- R_init
  {
    Mat_F2 <<-Mat_C/Mat_Cage # attention, initialisation de F2 au ratio de chaque metier
    indice <<-1
    nb_flot <<- ncol(Mat_C) # nombre de flottilles
    Mat_N2[indice] <<-init
    Mat_F2[indice,] <<-Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
    # réallocation du F estimé pour chaque métier
    Mat_q[indice,] <<-Mat_F2[indice,]/Mat_E[1,]
    Mat_N2[indice+1] <<-F_abundance(indice)

    for(compteur in 2:(age))
    {
      indice <<- compteur
      Mat_F2[indice,] <<- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
      Mat_q[indice,] <<- Mat_F2[indice,]/Mat_E[1,]
      if (indice<age) Mat_N2[indice+1] <<- F_abundance(indice)
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

  FT_pope <- 0.2
  Mat_Cage <<- apply(Mat_C,1,sum) # somme des captures de chaque flottille
  VPA_Pope(FT_pope, age, Mat_Cage, Mat_M) # Returns Mat_F
  Rinit <- Mat_N[1]

  MAT_N<<-rep(NA,age) # effectifs methode de Pope
  Mat_N2<<-rep(NA,age) # effectifs methode rectifiee
  Mat_C <<- Mat_C

  Mat_C <<- Mat_C
  Mat_E <<- Mat_E
  Mat_R <<- Mat_R
  Mat_M <<- Mat_M
  Mat_q <<- Mat_C #initialisation des capturabilites par les captures
  indice <<- 1 # age de recrutement
  optimize(F_pseudo_rectif_global,interval=c(Rinit/10,Rinit*10))$minimum

  plot(Mat_Cage, xlab = "Age", ylab = "Catch (numbers)", type = "b", main= paste0("Catch at age - ", espece))
  print("Mat_F2")
  print(Mat_F2)
  Mat_F <- apply(Mat_F2,1,sum)
  plot(Mat_F, xlab = "Age", ylab = "Fishing mortality (numbers)", type = "b", main= paste0("F at age - ", espece))
  print("Mat_N2")
  print(Mat_N2)
  plot(Mat_N2, xlab = "Age", ylab = "Abundance (numbers)", type = "b", main= paste0("Abundance at age - ", espece))

  varFT<-seq(0.2,1,by=0.2)
  matF3<-as.data.frame(matrix(NA, nrow = age, ncol = length(varFT)))
  colnames(matF3) <- varFT
  R_init<-rep(NA,length(varFT))

  for (i in 1:length(varFT)) {
    VPA_Pope(varFT[i], age, Mat_Cage, Mat_M)
    R_init[i]<-Mat_N[1]
    FT<-varFT[i]
    optimize(F_pseudo_rectif_global,interval=c(Rinit/10,Rinit*10))$minimum

    for (j in 1:age)
      if (nb_flot < 2)
      {
        matF3[j,i]<-Mat_F2[j,]
      }
    else
    {
      matF3[j,i]<-apply(Mat_F2[j,],1,sum)
    }
  }
  ylim_max <- max(matF3)+0.2

  plot(matF3[,1], xlab = "Age", ylab = "Fishing mortality", type = "l", ylim = c(0,ylim_max), main="Variation of F at age for different FT ")

  for (i in 2:length(varFT))
  {
    lines(matF3[,i], type = "l")
  }
  matF3$Age <- c(1:age)
  matF3 <- matF3 %>% pivot_longer(cols = c(1:5), values_to = "Fishing_mortality", names_to = "FT")
  matF3$FT <- as.factor(matF3$FT)
  theme_set(theme_bw())
  ggplot(data = matF3, aes(x = Age, y = Fishing_mortality, color = FT)) + geom_line()

}
