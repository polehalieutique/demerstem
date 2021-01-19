#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_5<- function (mcmc,data,n_proj){

  # ----------------------------------------------
  # Catches - predicted vs observed
  # ----------------------------------------------
  #windows()
  size.text <- 1.3
  size.labels <- 1
  box.size<-0.5
  def.par <- par(no.readonly = TRUE)
  Year<-data$Year
  n_obs <- length(Year)
  Year = c(Year, seq(from = max(Year), to = max(Year)+n_proj-1, by=1))
  #windows()

  par(mfrow = c(1,1), mar=c(5,5,1,1))

  col = c("lightgray","gray")

  # x = "C_pred"
  x = "C"

  #mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  n_plot <- dim(X)[2]

  q.mean <- apply(X,2,mean)
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

  plot(x=Year[1:n_plot],y=q.sup*1.2,ylim=c(0,max(q.sup*1.2)),ylab="Catches",type="n",las=1, main = "Fit Catches")
  polygon(x=c(Year[1:n_plot],rev(Year[1:n_plot])),y=c(q.sup,rev(q.inf)),col = col[1],border=NA)
  polygon(x=c(Year[1:n_plot],rev(Year[1:n_plot])),y=c(q.sup1,rev(q.inf1)),col = col[2],border=NA)
  points(x=Year[1:n_plot], y=q.mean, col="red", lwd=2, type="l")
  points(Year[1:n_plot],data$C_obs[1:n_plot],pch=20,cex=1,col="blue",type="p")

  legend(	legend = c("Cred interv. (95%)","C pred.","C obs."),
          col = c("lightgray","red","blue"),
          lty = c(3,2,NA), pch=c(NA,NA,20), lwd = c(2,2,NA), x = "topright", cex = size.text, bty ="n")

  # dev.off()



  # ----------------------------------------------
  # Abundance indices - predicted vs observed
  # ----------------------------------------------

  def.par <- par(no.readonly = TRUE)

  #windows()

  par(mfrow = c(1,1), mar=c(5,5,1,1))

  col = c("lightgray","gray")

  # x = "I_pred"
  x = "I"
  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  n_plot <- dim(X)[1]

  q.mean <- apply(X,1,mean)
  q.inf <- NULL
  q.sup <- NULL

}
