#'  Diagnosis on stock from F estimate
#'
#' \code{yield_recrut} function to be optimized by stats::optimize, hence gives an estimation by iteration of F minimum value
#'
#' @param   a         Weight-size relationship parameter
#' @param   b         Weight-size relationship parameter
#' @param   Linf      Asymptotic length
#' @param   K         Growth parameter
#' @param   t0        Growth parameter
#' @param   Mat_F     Vector of F
#' @param   Mat_W     Can be used instead to growth paramters
#' @param   mf        F multiplicator range
#'
#' @examples
#'
#' a <- 0.006
#' b <- 3.08
#'
#' Linf <- 44.4
#' K <- 0.35
#' t0 <- -0.24
#'
#' list_age <- seq(1, 10, by = 1)
#'
#' Mat_F <- c(40.102259e-05, 4.102259e-05, 5.102259e-05, 4.818126e-04 ,1.173966e-03, 20.173184e-02, 9.366615e-02, 2.059620e-01, 3.454309e-01, 2.906350e-01, 4.469356e-01, 3.430000e-01)
#' mf <- seq(0, 2, by=0.05)
#'
#' yield_recrut(a, b, Linf, K, t0, list_age, Mat_F, Mat_M, mf)
#'
#' @export

yield_recrut<- function(a, b, Linf, K, t0, list_age, Mat_F, Mat_M, mf, F0.1 = F, Mat_W = NA) {

  age.sim <- list_age #simulates ages

  age <- length(age.sim)
  Mat_N2 <- matrix(NA, nrow = age, ncol = length(mf))
  Mat_Y <- matrix(NA, nrow = age, ncol = length(mf))

  if (is.na(Mat_W[1]) == T) {
  Mat_W <- a *((Linf*(1-exp(-K*(age.sim-t0))))^b)/1000
  }

  Mat_N2[1,] <- 1 # Same recruitment
  for (i in 1:length(mf)){
    for (j in 2:age){
      Mat_N2[j,i]<-Mat_N2[j-1,i]*exp(-mf[i]*Mat_F[j-1]-Mat_M[j-1])
    }
  }

  for (i in 1:length(mf)){
    for (j in 1:age-1){
      Mat_Y[j,i]<-Mat_W[j]*(mf[i]*Mat_F[j]/(mf[i]*Mat_F[j]+Mat_M[j]))*Mat_N2[j,i]*(1-exp(-mf[i]*Mat_F[j]-Mat_M[j])) # W * Catches
    }
  }
  for (i in 1:length(mf)){
    Mat_Y[age,i]<-Mat_W[age]*(mf[i]*Mat_F[age]/(mf[i]*Mat_F[age]+Mat_M[age]))*Mat_N2[age,i] # taking account of catch in group +
  }
  Mat_Ytot <- apply(Mat_Y,2,sum)
  par(mar=c(5, 4, 4, 6) + 0.1)
  plot(mf,Mat_Ytot, type = "l", lwd = 2,xlab="", ylab="", ylim =c(0, max(Mat_Ytot) + max(Mat_Ytot)/10), main = c(" - Yield per recruit curve - ", espece), axes = F)
  axis(2, ylim=c(0,1),col="black",las=1)
  mtext("Yield per recruit",side=2,line=2.5)
  box()
  abline(v=1, h= Mat_Ytot[match(1,mf)] , col = 'red', lwd = 1)
  if (F0.1 == F) {
    abline(v= mf[match(max(Mat_Ytot), Mat_Ytot)], h= max(Mat_Ytot) , col = 'red', lwd = 1, lty = 2)
    legend(legend = c("Present", "Maximization effort"),col = c("red","red"), lty = c(1,2), lwd = c(1,1), x = "bottomright", cex = 1, bty ="n")

    }
  else {
    res <- (round((Mat_Ytot[match(mf[2]-mf[1],mf)]) / (mf[2]-mf[1]), digit = 2))/10
    Mat_Ytot_B <- Mat_Ytot[-1]
    Mat_Ytot_A <- Mat_Ytot[-(length(mf))]
    diff <- (Mat_Ytot_B - Mat_Ytot_A) / (mf[2]-mf[1])
    abline(h= Mat_Ytot[which.min(abs(diff - res))+1], v= mf[which.min(abs(diff - res))+1] , col = 'red', lwd = 1, lty = 2)
    legend(legend = c("Present", "F0.1"),col = c("red","red"), lty = c(1,2), lwd = c(1,1), x = "bottomright", cex = 1, bty ="n")
  }


  B4 <- as.data.frame(Mat_N2) * Mat_W
  Btot <- apply(B4,2,sum)
  par(new=TRUE)

  ## Plot the second plot and put axis scale on right
  plot(mf,Btot, xlab="", ylab="", type = "l", lwd = 2, col = "blue", ylim = c(0, max(Btot)), axes=FALSE) #min(Btot)
  ## a little farther out (line=4) to make room for labels
  mtext("Biomass",side=4,col="blue",line=2.5)
  axis(4, col="blue",col.axis="blue",las=1, ylim = c(min(Btot), max(Btot)))

  ## Draw the time axis
  axis(1, ylim=c(min(mf),max(mf)),col="black",las=1)
  mtext("Effort multiplier",side=1,col="black",line=2)

  ## Add Legend
  # legend("topleft",legend=c("Beta Gal","Cell Density"),
  #        text.col=c("black","red"),pch=c(16,15),col=c("black","red"))

  # plot(mf*mean(Mat_F),Mat_Ytot, xlab = paste0("Fishing Mortality (ages 1-", age,")"), ylab = "Yield per recruit", type = "l", ylim =c(0, max(Mat_Ytot) + max(Mat_Ytot)/10), main = c(" - Yield per recruit curve - ", espece))
  # abline(v=mean(Mat_F), h= Mat_Ytot[match(1,mf)] , col = 'blue', lwd = 2)
  # abline(v= mf[match(max(Mat_Ytot), Mat_Ytot)], h= max(Mat_Ytot) , col = 'red', lwd = 2, lty = 2)
  # legend(legend = c("Present", "Maximization effort"),col = c("blue","red"), lty = c(1,2), lwd = c(2,2), x = "bottomright", cex = 1, bty ="n")

  }
