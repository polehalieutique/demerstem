#'  yield_recrut
#'
#' \code{yield_recrut} function to be optimized by stats::optimize, hence gives an estimation by iteration of F minimum value
#'
#' @param   a          Initial value for pseudo cohort analysis, equivalent to the last year recruitment
#' @param   b         data frame of catch, each column is a different gear
#' @param   Linf         vector of recruitment, if unknown, considered constant
#' @param   K         data frame of effort, each column is a different gear
#' @param   t0         vector of mortality biological parameter
#' @param   Mat_N2
#' @param   Mat_Y
#' @param   Mat_F3
#'
#' @examples
#' espece <- "dicentrarchus labrax"
#' age <- 18
#' Mat_C <- data.frame(catchA = c(8, 438, 13955, 16933, 47864, 22925,23117,4684,15853,10942,8839,18524,8141,9850,4485,3608,2642,1310), catchB = c(0, 184, 1827, 2245, 7158, 4792,7151,1767,5378,3524,1429,2533,1481,2470,1161,563,617,477), catchC = c(8, 1103, 11285, 5938, 9773, 2749,1722,228,678,483,368,213,87,265,224,87,85,0), catchD = c(7, 6435, 20817, 4023, 3656, 905,570,117,131,60,115,42,111,157,117,0,51,0))
#' Mat_Cage <- apply(Mat_C, 1, sum) # catch sum by age over fleets
#' Mat_M <- rep(0.2, age)
#' FT <- 0.2
# # VPA_Pope(FT_init = FT, age, Mat_Cage, Mat_M)
# # Mat_N[1]
#
#' Mat_E <- data.frame(effortA = c(3500, 2900, 2200, 1500, 800, 800,800,800,800,800,800,800,800,800,800,800,800,800), effortB = c(800, 600, 450, 350, 200, 50,50,0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0), efforC = c(550, 400, 200, 100, 100, 100,100,100,100,100,100,100,100,100,100,100,100,100), effortD = c(430, 600, 760, 900, 900, 1000,1100,1150,1250,1350,1350,1350,1350,1350,1350,1350,1350,1350))
#' Mat_R <- rep(100000, 18) # si indisponible, on suppose recrutement constant
#' Mat_M <- rep(0.2, 18)
#' Rinit <- 1.18*10^6#  Mat_N[1] # valeur issue de Bertignac, 1987
#' pseudo_rectif_R(Mat_Cage, Mat_C, Mat_R, Mat_E, Mat_M, Rinit)
#' b <- 3.1428
#' a <- 0.0054
#' Linf<- 77.2
#' K<- 0.16
#' t0 <- -0.7
#' @export

yield_recrut<- function(a, b, Linf, K, t0, age, Mat_F3, Mat_M, mf) {
  Mat_N2 <- matrix(NA, nrow = age, ncol = length(mf))
  Mat_Y <- matrix(NA, nrow = age, ncol = length(mf))
  age.sim <- age #simulates ages
  Mat_W <- a *((Linf*(1-exp(-K*(age.sim-t0))))^b)/1000

  Mat_N2[1,] <- 1
  for (i in 1:length(mf)){
    for (j in 2:age){
      Mat_N2[j,i]<-Mat_N2[j-1,i]*exp(-mf[i]*Mat_F3[j-1]-Mat_M[j-1])
    }
  }

  for (i in 1:length(mf)){
    for (j in 1:age-1){
      Mat_Y[j,i]<-Mat_W[j]*(mf[i]*Mat_F3[j]/(mf[i]*Mat_F3[j]+Mat_M[j]))*Mat_N2[j,i]*(1-exp(-mf[i]*Mat_F3[j]-Mat_M[j]))
    }
  }
  for (i in 1:length(mf)){
    Mat_Y[age,i]<-Mat_W[age]*(mf[i]*Mat_F3[age]/(mf[i]*Mat_F3[age]+Mat_M[age]))*Mat_N2[age,i] # taking account of catch in group +
  }
  Mat_Ytot <- apply(Mat_Y,2,sum)
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(mf,Mat_Ytot, type = "l", lwd = 2,xlab="", ylab="", ylim =c(0, max(Mat_Ytot) + max(Mat_Ytot)/10), main = c(" - Yield per recruit curve - ", espece), axes = F)
  axis(2, ylim=c(0,1),col="black",las=1)
  mtext("Yield per recruit",side=2,line=2.5)
  box()
  abline(v=1, h= Mat_Ytot[match(1,mf)] , col = 'red', lwd = 1)
  abline(v= mf[match(max(Mat_Ytot), Mat_Ytot)], h= max(Mat_Ytot) , col = 'red', lwd = 1, lty = 2)
  legend(legend = c("Present", "Maximization effort"),col = c("red","red"), lty = c(1,2), lwd = c(1,1), x = "bottomright", cex = 1, bty ="n")

  B4 <- as.data.frame(Mat_N2) * Mat_W
  Btot <- apply(B4,2,sum)
  par(new=TRUE)

  ## Plot the second plot and put axis scale on right
  plot(mf,Btot, xlab="", ylab="", type = "l", lwd = 2, col = "blue", ylim = c(min(Btot), max(Btot)), axes=FALSE)
  ## a little farther out (line=4) to make room for labels
  mtext("Biomass",side=4,col="blue",line=2.5)
  axis(4, col="blue",col.axis="blue",las=1, ylim = c(min(Btot), max(Btot)))

  ## Draw the time axis
  axis(1, ylim=c(min(mf),max(mf)),col="black",las=1)
  mtext("Effort multiplier",side=1,col="black",line=2)

  ## Add Legend
  # legend("topleft",legend=c("Beta Gal","Cell Density"),
  #        text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

  # plot(mf*mean(Mat_F3),Mat_Ytot, xlab = paste0("Fishing Mortality (ages 1-", age,")"), ylab = "Yield per recruit", type = "l", ylim =c(0, max(Mat_Ytot) + max(Mat_Ytot)/10), main = c(" - Yield per recruit curve - ", espece))
  # abline(v=mean(Mat_F3), h= Mat_Ytot[match(1,mf)] , col = 'blue', lwd = 2)
  # abline(v= mf[match(max(Mat_Ytot), Mat_Ytot)], h= max(Mat_Ytot) , col = 'red', lwd = 2, lty = 2)
  # legend(legend = c("Present", "Maximization effort"),col = c("blue","red"), lty = c(1,2), lwd = c(2,2), x = "bottomright", cex = 1, bty ="n")

  }
