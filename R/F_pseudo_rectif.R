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

  # Get Mat_F
  R_pseudo_rectif_bis <-function(init) # init <- R_init
  {
    Mat_F2 <<-Mat_C/Mat_Cage # initialisation of F2 as metier ratio (= 1 if only one metier/fleet)
    indice <<-1
    nb_flot <<- ncol(Mat_C) # fleet size
    Mat_N2[indice] <<-init
    Mat_F2[indice,] <<-Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum # Get F as (C_obs - C_th) is small small
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

  # get Rinit estimation to restrict iterations to an intervall [Rinit; 10*Rinit]
  FT_pope <- 0.2
  Mat_Cage <<- apply(Mat_C,1,sum) # somme des captures de chaque flottille
  VPA_Pope(FT_pope, age, Mat_Cage, Mat_M) # Returns Mat_F
  Rinit <- Mat_N[1]

  MAT_N<<-rep(NA,age) # effectifs methode de Pope
  Mat_N2<<-rep(NA,age) # effectifs methode rectifiee

  Mat_C <<- Mat_C
  Mat_E <<- Mat_E
  Mat_R <<- Mat_R
  Mat_M <<- Mat_M
  Mat_q <<- Mat_C #initialisation des capturabilites par les captures
  indice <<- 1 # age de recrutement
  optimize(F_pseudo_rectif_global,interval=c(Rinit/10,Rinit*10))$minimum # Get Mat_F as FT = 0.2

  Mat_Cage_plot <- as.data.frame(Mat_Cage)[1]
  Mat_Cage_plot$Age <- list_age
  Mat_Cage_plot$title <- paste0("Catch at age - ", espece)
  plot <- ggplot(data = Mat_Cage_plot, aes(x = Age, y = Mat_Cage)) + labs(x = "Age", y = "Catch") + geom_line(size = 1.01)+ geom_point() + theme_nice() + facet_grid(~title)
  print(plot)

  print("Mat_F2")
  print(Mat_F2)
  Mat_F <- apply(Mat_F2,1,sum)

  Mat_F_plot <- as.data.frame(Mat_F)
  Mat_F_plot$Age <- list_age
  Mat_F_plot$title <- paste0("F at age - ", espece)
  plot <- ggplot(data = Mat_F_plot, aes(x = Age, y = Mat_F)) + labs(x = "Age", y = "Fishing mortality") + geom_line(size = 1)+ geom_point() + theme_nice() + facet_grid(~title)
  print(plot)

  print("Mat_N2")
  print(Mat_N2)

  Mat_N2_plot <- as.data.frame(Mat_N2)
  Mat_N2_plot$Age <- list_age
  Mat_N2_plot$title <- paste0("Abdundance at age - ", espece)
  plot <- ggplot(data = Mat_N2_plot, aes(x = Age, y = Mat_N2)) + labs(x = "Age", y = "Abundance") + geom_line(size = 1) + geom_point() + theme_nice() + facet_grid(~title)
  print(plot)

  varFT<-seq(0.2,1,by=0.2)
  matF3<-as.data.frame(matrix(NA, nrow = age, ncol = length(varFT)))
  colnames(matF3) <- varFT
  R_init<-rep(NA,length(varFT))

  # Repeat for different values of F (0 -> 1)
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

  matF3$Age <- list_age
  matF3 <- matF3 %>% pivot_longer(cols = c(1:5), values_to = "Fishing_mortality", names_to = "FT")
  matF3$FT <- as.factor(matF3$FT)
  matF3$title <- "Variation of F at age for different FT"
  plot <- ggplot(data = matF3, aes(x = Age, y = Fishing_mortality, color = FT)) + geom_line(size = 1.01) + theme_nice() + facet_grid(~title)
  print(plot)
}
