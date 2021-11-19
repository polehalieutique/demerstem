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
#' @param   Mat_F2
#'
#' @examples
#'  data(tableau_sc)
#'  model_ai_plus(tableau_sc, esp = "PSEUDOTOLITHUS ELONGATUS", effort = "auto", title = "SC", list_param = c("annee", "saison", "strate"), var_eff_list=c("surface_chalutee"), espece_id='nom_taxonomique', catch_col='total_capture', interactions =FALSE, limit=0.001, formula_select = "log(i_ab+0.0001) ~ strate + annee + saison")
#'
#' @export

yield_recrut<- function(a, b, Linf, K, t0, age, Mat_N2, Mat_Y, Mat_F2) {
  Winf <- a*(Linf^b)
  age.sim <- seq(1,age,by = 1) #simulates ages
  Mat_W <- (Winf*(1-exp(-K*(age.sim-t0)))^b)/1000

  Mat_N2[1,] <- 1
  for (i in 1:length(mf)){
    for (j in 2:age){
      Mat_N2[j,i]<-Mat_N2[j-1,i]*exp(-mf[i]*Mat_F2[j-1]-Mat_M[j-1])
    }
  }

  for (i in 1:length(mf)){
    for (j in 1:age-1){
      Mat_Y[j,i]<-Mat_W[j]*(mf[i]*Mat_F2[j]/(mf[i]*Mat_F2[j]+Mat_M[j]))*Mat_N2[j,i]*(1-exp(-mf[i]*Mat_F2[j]-Mat_M[j]))
    }
  }
  for (i in 1:length(mf)){
    Mat_Y[age,i]<-Mat_W[age]*(mf[i]*Mat_F2[age]/(mf[i]*Mat_F2[age]+Mat_M[age]))*Mat_N2[age,i] # taking account of catch in group +
  }
  Mat_Ytot <- apply(Mat_Y,2,sum)

  plot(mf,Mat_Ytot, xlab = "Effort multiplier", ylab = "Yield per recruit", type = "l", ylim =c(0, max(Mat_Ytot) + max(Mat_Ytot)/10), main = c(" - Yield per recruit curve - ", espece))
  abline(v=1, h= Mat_Ytot[match(1,mf)] , col = 'blue', lwd = 2)
  abline(v= mf[match(max(Mat_Ytot), Mat_Ytot)], h= max(Mat_Ytot) , col = 'red', lwd = 2, lty = 2)
  legend(legend = c("Present", "Maximization effort"),col = c("blue","red"), lty = c(1,2), lwd = c(2,2), x = "bottomright", cex = 1, bty ="n")

  # plot(mf*mean(Mat_F2),Mat_Ytot, xlab = paste0("Fishing Mortality (ages 1-", age,")"), ylab = "Yield per recruit", type = "l", ylim =c(0, max(Mat_Ytot) + max(Mat_Ytot)/10), main = c(" - Yield per recruit curve - ", espece))
  # abline(v=mean(Mat_F2), h= Mat_Ytot[match(1,mf)] , col = 'blue', lwd = 2)
  # abline(v= mf[match(max(Mat_Ytot), Mat_Ytot)], h= max(Mat_Ytot) , col = 'red', lwd = 2, lty = 2)
  # legend(legend = c("Present", "Maximization effort"),col = c("blue","red"), lty = c(1,2), lwd = c(2,2), x = "bottomright", cex = 1, bty ="n")

  }
