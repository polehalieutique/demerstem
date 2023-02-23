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

F_pseudo_rectif <- function(FT, age, Mat_C, Mat_R, Mat_E, Mat_M, step_time, Prop_R, quarter, ngroup= F) {

  plot <- list()
  FT_init <- FT
  Mat_q <<- Mat_C #initialisation des capturabilites par les captures
  Mat_N<<-rep(NA,age) # effectifs methode de Pope
  Mat_N2<<-rep(NA,age) # effectifs methode rectifiee
  Mat_Cage <<- apply(Mat_C,1,sum)

  # Get Mat_F
  R_pseudo_rectif_bis <-function(init) # init <- Rinit
  {
    Mat_F2 <<- (Mat_C/Mat_Cage)#/step_time # initialisation of F2 as metier ratio (= 1 if only one metier/fleet)

    nb_flot <<- ncol(Mat_C) # fleet size

    if (quarter == T) {
      print(init)
      Mat_N2[1] <-round(init * (Prop_R[1])) # N1
      assign('Mat_N2', Mat_N2, envir=globalenv())

      indice <<-1

      Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum

      assign('Mat_F2', Mat_F2, envir=globalenv())
      Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
      assign('Mat_q', Mat_q, envir=globalenv())

      if (nb_flot < 2) {

        Mat_N2[2] <- Mat_N2[1] * exp(-(Mat_q[1, ] * Mat_E[4, ]) -  Mat_M[1]) + init*Prop_R[2]
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 2
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[3] <- Mat_N2[2] * exp(-(Mat_q[2, ] * Mat_E[3, ]) -  Mat_M[2]) + init*Prop_R[3]
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 3
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[4] <- Mat_N2[3] * exp(-(Mat_q[3, ] * Mat_E[2, ]) -  Mat_M[3]) + init*Prop_R[4]
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 4
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        NT2 <- Mat_N2[1] * Mat_R[2]/Mat_R[1] * exp(-(Mat_q[1, ] * Mat_E[8, ]) -  Mat_M[1]) + init*Prop_R[2]
        NT3 <- NT2 *                           exp(-(Mat_q[2, ] * Mat_E[7, ]) -  Mat_M[2]) + init*Prop_R[3]
        NT4 <- NT3 *                           exp(-(Mat_q[3, ] * Mat_E[6, ]) -  Mat_M[3]) + init*Prop_R[4]

        Mat_N2[5] <- NT4 * exp(-(Mat_q[4, ] * Mat_E[5, ]) -  Mat_M[4])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 5
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[6] <- Mat_N2[5] * exp(-(Mat_q[5, ] * Mat_E[4, ]) -  Mat_M[5])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 6
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[7] <- Mat_N2[6] * exp(-(Mat_q[6, ] * Mat_E[3, ]) -  Mat_M[6])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 7
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())


        Mat_N2[8] <- Mat_N2[7] * exp(-(Mat_q[7, ] * Mat_E[2, ]) -  Mat_M[7])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 8
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        NT2 <- Mat_N2[1] * Mat_R[3]/Mat_R[1] * exp(-(Mat_q[1, ] * Mat_E[12, ]) -  Mat_M[1]) + init*Prop_R[2]
        NT3 <- NT2 *                           exp(-(Mat_q[2, ] * Mat_E[11, ]) -  Mat_M[2]) + init*Prop_R[3]
        NT4 <- NT3 *                           exp(-(Mat_q[3, ] * Mat_E[10, ]) -  Mat_M[3]) + init*Prop_R[4]


        NT5 <- NT4 * exp(-(Mat_q[4, ] * Mat_E[9, ]) -  Mat_M[4])
        NT6 <- Mat_N2[5] * exp(-(Mat_q[5, ] * Mat_E[8, ]) -  Mat_M[5])
        NT7 <- Mat_N2[6] * exp(-(Mat_q[6, ] * Mat_E[7, ]) -  Mat_M[6])
        NT8 <- Mat_N2[7] * exp(-(Mat_q[7, ] * Mat_E[6, ]) -  Mat_M[7])


        Mat_N2[9] <- NT8 * exp(-(Mat_q[8, ] * Mat_E[5, ]) -  Mat_M[8])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 9
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[10] <- Mat_N2[9] * exp(-(Mat_q[9, ] * Mat_E[4, ]) -  Mat_M[9])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 10
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[11] <- Mat_N2[10] * exp(-(Mat_q[10, ] * Mat_E[3, ]) -  Mat_M[10])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 11
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[12] <- Mat_N2[11] * exp(-(Mat_q[11, ] * Mat_E[2, ]) -  Mat_M[11])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 12
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        assign('Mat_N2', Mat_N2, envir=globalenv())
      }
      else {
        Mat_N2[2] <- Mat_N2[1] * exp(-apply((Mat_q[1, ] * Mat_E[4, ]), 1, sum) -  Mat_M[1]) + init*Prop_R[2]
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 2

        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum

        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[3] <- Mat_N2[2] * exp(-apply((Mat_q[2, ] * Mat_E[3, ]), 1, sum) -  Mat_M[2]) + init*Prop_R[3]
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 3
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum

        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[4] <- Mat_N2[3] * exp(-apply((Mat_q[3, ] * Mat_E[2, ]), 1, sum) -  Mat_M[3]) + init*Prop_R[4]
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 4
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())


        NT2 <- Mat_N2[1] * Mat_R[2]/Mat_R[1] * exp(-apply((Mat_q[1, ] * Mat_E[8, ]), 1, sum) -  Mat_M[1]) + init*Prop_R[2]
        NT3 <- NT2 *                           exp(-apply((Mat_q[2, ] * Mat_E[7, ]), 1, sum) -  Mat_M[2]) + init*Prop_R[3]
        NT4 <- NT3 *                           exp(-apply((Mat_q[3, ] * Mat_E[6, ]), 1, sum) -  Mat_M[3]) + init*Prop_R[4]

        Mat_N2[5] <- NT4 * exp(-apply((Mat_q[4, ] * Mat_E[5, ]), 1, sum) -  Mat_M[4])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 5
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[6] <- Mat_N2[5] * exp(-apply((Mat_q[5, ] * Mat_E[4, ]), 1, sum) -  Mat_M[5])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 6
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[7] <- Mat_N2[6] * exp(-apply((Mat_q[6, ] * Mat_E[3, ]), 1, sum) -  Mat_M[6])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 7
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[8] <- Mat_N2[7] * exp(-apply((Mat_q[7, ] * Mat_E[2, ]), 1, sum) -  Mat_M[7])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 8
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())


        NT2 <- Mat_N2[1] * Mat_R[3]/Mat_R[1] * exp(-apply((Mat_q[1, ] * Mat_E[12, ]), 1, sum) -  Mat_M[1]) + init*Prop_R[2]
        NT3 <- NT2 *                           exp(-apply((Mat_q[2, ] * Mat_E[11, ]), 1, sum) -  Mat_M[2]) + init*Prop_R[3]
        NT4 <- NT3 *                           exp(-apply((Mat_q[3, ] * Mat_E[10, ]), 1, sum) -  Mat_M[3]) + init*Prop_R[4]


        NT5 <- NT4 * exp(-apply((Mat_q[4, ] * Mat_E[9, ]), 1, sum) -  Mat_M[4])
        NT6 <- NT5 * exp(-apply((Mat_q[5, ] * Mat_E[8, ]), 1, sum) -  Mat_M[5])
        NT7 <- NT6 * exp(-apply((Mat_q[6, ] * Mat_E[7, ]), 1, sum) -  Mat_M[6])
        NT8 <- NT7 * exp(-apply((Mat_q[7, ] * Mat_E[6, ]), 1, sum) -  Mat_M[7])


        Mat_N2[9] <- NT8 * exp(-apply((Mat_q[8, ] * Mat_E[5, ]), 1, sum) -  Mat_M[8])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 9
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[10] <- Mat_N2[9] * exp(-apply((Mat_q[9, ] * Mat_E[4, ]), 1, sum) -  Mat_M[9])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 10
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[11] <- Mat_N2[10] * exp(-apply((Mat_q[10, ] * Mat_E[3, ]), 1, sum) -  Mat_M[10])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 11
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        Mat_N2[12] <- Mat_N2[11] * exp(-apply((Mat_q[11, ] * Mat_E[2, ]), 1, sum) -  Mat_M[11])
        assign('Mat_N2', Mat_N2, envir=globalenv())

        indice <<- 12
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())

        assign('Mat_N2', Mat_N2, envir=globalenv())

        if(ngroup == 4){

          NT2 <- Mat_N2[1] * Mat_R[4]/Mat_R[1] * exp(-apply((Mat_q[1, ] * Mat_E[16, ]), 1, sum) -  Mat_M[1]) + init*Prop_R[2]
          NT3 <- NT2 *                           exp(-apply((Mat_q[2, ] * Mat_E[15, ]), 1, sum) -  Mat_M[2]) + init*Prop_R[3]
          NT4 <- NT3 *                           exp(-apply((Mat_q[3, ] * Mat_E[14, ]), 1, sum) -  Mat_M[3]) + init*Prop_R[4]


          NT5 <- NT4 * exp(-apply((Mat_q[4, ] * Mat_E[13, ]), 1, sum) -  Mat_M[4])
          NT6 <- NT5 * exp(-apply((Mat_q[5, ] * Mat_E[12, ]), 1, sum) -  Mat_M[5])
          NT7 <- NT6 * exp(-apply((Mat_q[6, ] * Mat_E[11, ]), 1, sum) -  Mat_M[6])
          NT8 <- NT7 * exp(-apply((Mat_q[7, ] * Mat_E[10, ]), 1, sum) -  Mat_M[7])


          NT9  <- NT8 * exp(-apply((Mat_q[8, ] * Mat_E[9, ]), 1, sum) -  Mat_M[8])
          NT10 <- NT9 * exp(-apply((Mat_q[9, ] * Mat_E[8, ]), 1, sum) -  Mat_M[9])
          NT11 <- NT10 * exp(-apply((Mat_q[10, ] * Mat_E[7, ]), 1, sum) -  Mat_M[10])
          NT12 <- NT11 * exp(-apply((Mat_q[11, ] * Mat_E[6, ]), 1, sum) -  Mat_M[11])


          Mat_N2[13] <- NT12 * exp(-apply((Mat_q[12, ] * Mat_E[5, ]), 1, sum) -  Mat_M[12])
          assign('Mat_N2', Mat_N2, envir=globalenv())

          indice <<- 13
          Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
          assign('Mat_F2', Mat_F2, envir=globalenv())
          Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
          assign('Mat_q', Mat_q, envir=globalenv())

          Mat_N2[14] <- Mat_N2[13] * exp(-apply((Mat_q[13, ] * Mat_E[4, ]), 1, sum) -  Mat_M[13])
          assign('Mat_N2', Mat_N2, envir=globalenv())

          indice <<- 14
          Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
          assign('Mat_F2', Mat_F2, envir=globalenv())
          Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
          assign('Mat_q', Mat_q, envir=globalenv())

          Mat_N2[15] <- Mat_N2[14] * exp(-apply((Mat_q[14, ] * Mat_E[3, ]), 1, sum) -  Mat_M[14])
          assign('Mat_N2', Mat_N2, envir=globalenv())

          indice <<- 15
          Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
          assign('Mat_F2', Mat_F2, envir=globalenv())
          Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
          assign('Mat_q', Mat_q, envir=globalenv())

          Mat_N2[16] <- Mat_N2[15] * exp(-apply((Mat_q[15, ] * Mat_E[2, ]), 1, sum) -  Mat_M[15])
          assign('Mat_N2', Mat_N2, envir=globalenv())

          indice <<- 16
          Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
          assign('Mat_F2', Mat_F2, envir=globalenv())
          Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
          assign('Mat_q', Mat_q, envir=globalenv())

          assign('Mat_N2', Mat_N2, envir=globalenv())

        }

      }
    }

    else{
      Mat_F2 <<- (Mat_C/Mat_Cage)#/step_time # initialisation of F2 as metier ratio (= 1 if only one metier/fleet)
      indice <<-1
      nb_flot <<- ncol(Mat_C) # fleet size

      Mat_N2[indice] <- init
      assign('Mat_N2', Mat_N2, envir=globalenv())
      Mat_F2[indice,] <-Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum # Equation de ventilation : optimize (C_th - C_obs) and returns an approximation of F, used to multiply F2
      assign('Mat_F2', Mat_F2, envir=globalenv())
      # reallocating estimated F by fishery/gear
      Mat_q[indice,] <-Mat_F2[indice,]/Mat_E[1,]
      assign('Mat_q', Mat_q, envir=globalenv())
      Mat_N2[indice+1] <-F_abundance(indice)
      assign('Mat_N2', Mat_N2, envir=globalenv())
      for(compteur in 2:(age))
      {
        indice <<- compteur
        Mat_F2[indice,] <- Mat_F2[indice,]*optimize(F_a_min,interval=c(0,2))$minimum
        assign('Mat_F2', Mat_F2, envir=globalenv())
        Mat_q[indice,] <- Mat_F2[indice,]/Mat_E[1,]
        assign('Mat_q', Mat_q, envir=globalenv())
        if (indice<age) {
          Mat_N2[indice+1] <- F_abundance(indice)
          assign('Mat_N2', Mat_N2, envir=globalenv())
        }
      }
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


  F_pseudo_rectif_global<-function(init) # Return difference between FT and R_pseudo FT and by extracting the minimum using an optimize function, it will return results from an initialization with a FT
  {
    res<-(FT-R_pseudo_rectif_bis(init))^2
    res
  }

  ####
  # Repeat for different values of F (0 -> 1)
  ####
  varFT<-seq(0.2/step_time,1/step_time,by=0.2/step_time)
  matF3<-as.data.frame(matrix(NA, nrow = age, ncol = length(varFT)))
  colnames(matF3) <- varFT
  Rinit<-rep(NA,length(varFT))

  for (i in 1:length(varFT)) {
    VPA_Pope(varFT[i], age, Mat_Cage, Mat_M)
    Rinit<-Mat_N[1]
    FT <- varFT[i]
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

  ####
  # TRUE beginning of function, Mat_F is updated with final calculated # this is a cheap trick because I have not enough time
  ####

  # get Rinit estimation to restrict iterations to an intervall [Rinit; 10*Rinit]
  FT<- FT_init
  Mat_Cage <<- apply(Mat_C,1,sum) # somme des captures de chaque flottille
  VPA_Pope(FT, age, Mat_Cage, Mat_M) # Returns Mat_F
  Rinit <- Mat_N[1]

  # Mat_N<<-rep(NA,age) # effectifs methode de Pope
  # Mat_N2<<-rep(NA,age) # effectifs methode rectifiee

  # Mat_C <<- Mat_C
  # Mat_E <<- Mat_E
  # Mat_R <<- Mat_R
  # Mat_M <<- Mat_M

  indice <<- 1 # age de recrutement
  optimize(F_pseudo_rectif_global,interval=c(Rinit/10,Rinit*10))$minimum # Examine for interval on R_init which one optimize FT

  Mat_Cage_plot <- as.data.frame(Mat_Cage)[1]
  Mat_Cage_plot$Age <- list_age
  Mat_Cage_plot$title <- paste0("Catch at age - ", espece)
  plot[[1]] <- ggplot(data = Mat_Cage_plot, aes(x = Age, y = Mat_Cage)) + labs(x = "Age", y = "Catch") + geom_line(size = 1.01)+ geom_point() + theme_nice() + facet_grid(~title)
  print(plot[[1]])

  print(paste0("Matrice des mortalités par pêche F pour FT = ",FT))
  Mat_F <- apply(Mat_F2,1,sum)
  assign('Mat_F', Mat_F, envir=globalenv())
  print(Mat_F)

  Mat_F_plot <- as.data.frame(Mat_F)
  Mat_F_plot$Age <- list_age
  Mat_F_plot$title <- paste0("F at age - ", espece)
  Mat_F_plot$F_mean[1] <- mean(c(Mat_F_plot$Mat_F[1], Mat_F_plot$Mat_F[1], Mat_F_plot$Mat_F[2]), na.rm=T)
  z <- nrow(Mat_F_plot)
  Mat_F_plot$F_mean[z] <- mean(c(Mat_F_plot$Mat_F[z], Mat_F_plot$Mat_F[z], Mat_F_plot$Mat_F[z-1]), na.rm=T)
  for (i in 2:(nrow(Mat_F_plot)-1)){
    Mat_F_plot$F_mean[i] <- mean(c(Mat_F_plot$Mat_F[i-1], Mat_F_plot$Mat_F[i], Mat_F_plot$Mat_F[i+1]), na.rm=T)
  }
  plot[[2]] <- ggplot(data = Mat_F_plot) +
    labs(x = "Age", y = "Fishing mortality") + geom_line(aes(x = Age, y = Mat_F)) + geom_line(size = 1, aes(x = Age, y = F_mean)) +
    geom_point(aes(x = Age, y = F_mean)) + theme_nice() + facet_grid(~title)

  # if(length(Mat_C) > 1) { # add Mat_F2 to plot[[2]] with F for each fishery
  #   Mat_F2_plot <- as.data.frame(Mat_F2)
  #   Mat_F2_plot$Age <- list_age
  #   Mat_F2_plot <- Mat_F2_plot %>%  pivot_longer(cols=c(1,2),names_to = "Fleet", values_to = "Mat_F")
  #   plot[[2]] <- plot[[2]] + geom_line(data = Mat_F2_plot, aes(x = Age, y = Mat_F, col = Fleet, linetype = Fleet), size = 1.05)
  # }
  print(plot[[2]])

  print("Mat_N2")
  print(Mat_N2)

  Mat_N2_plot <- as.data.frame(Mat_N2)
  Mat_N2_plot$Age <- list_age
  Mat_N2_plot$title <- paste0("Abdundance at age - ", espece)
  plot[[3]] <- ggplot(data = Mat_N2_plot, aes(x = Age, y = Mat_N2)) + ylim(c(0, max(Mat_N2))) + labs(x = "Age", y = "Abundance") + geom_line(size = 1) + geom_point() + theme_nice() + facet_grid(~title)
  print(plot[[3]])


  plot[[4]] <- ggplot(data = matF3, aes(x = Age, y = Fishing_mortality, color = FT)) + geom_line(size = 1.01) + theme_nice() + facet_grid(~title)
  print(plot[[4]])
  return(plot)
}
