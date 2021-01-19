#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_4<- function (mcmc,data,n_proj){

  # ----------------------------------------------------------------------------
  # Equilibrium curve
  # ----------------------------------------------------------------------------

  #windows()
  size.text <- 1.3
  size.labels <- 1
  box.size<-0.5
  x = "C_e"
  B_e <- seq(from = 0, to = 1500, by = 50)
  n_equi <- length(B_e)

  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

  col = c("lightgray","gray")

  q.med <- apply(X,2,median)

  q.inf <- NULL
  q.sup <- NULL
  for (k in 1:dim(X)[2])
  {
    q.inf[k] <- quantile(X[,k],probs=0.05)
    q.sup[k] <- quantile(X[,k],probs=0.95)
  }

  q.inf1 <- NULL
  q.sup1 <- NULL
  for (k in 1:dim(X)[2])
  {
    q.inf1[k] <- quantile(X[,k],probs=0.25)
    q.sup1[k] <- quantile(X[,k],probs=0.75)
  }

  plot(x=B_e,y=q.sup*2,ylab="Equilibrium catches",xlab="Biomass at equilibrium",type="n")
  polygon(x=c(B_e,rev(B_e)),y=c(q.sup,rev(q.inf)),col=col[1],border=NA)
  polygon(x=c(B_e,rev(B_e)),y=c(q.sup1,rev(q.inf1)),col = col[2],border=NA)
  points(x=B_e,y=q.med,yaxt="n",col="red",lwd=2,type="l")

  q_C_MSY <- quantile(mcmc.table$C_MSY,probs=c(0.05,0.5,0.95))
  q_B_MSY <- quantile(mcmc.table$B_MSY,probs=c(0.05,0.5,0.95))

  abline(v=q_B_MSY[2], col = "green", lty=4, lwd = 3)
  abline(v=q_B_MSY[1], col = "green", lty=4, lwd = 1)
  abline(v=q_B_MSY[3], col = "green", lty=4, lwd = 1)

  abline(h=q_C_MSY[2], col = "green", lty=4, lwd = 3)
  abline(h=q_C_MSY[1], col = "green", lty=4, lwd = 1)
  abline(h=q_C_MSY[3], col = "green", lty=4, lwd = 1)

  x = "B"
  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  B.med <- apply(X,2,median)

  points(B.med,data$C_obs[1:length(B.med)], type = "b", col="blue", pch=22)

}
