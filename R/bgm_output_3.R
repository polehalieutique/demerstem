#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_3<- function (mcmc,data,n_proj=0){

  # ----------------------------------------------
  # Harvest Rate
  # ----------------------------------------------

  Year<-data$Year
  n_obs <- length(Year)
  if (n_proj>0) {Year = c(Year, seq(from = max(Year), to = max(Year)+n_proj-1, by=1))}
  #windows()
  size.text <- 1.3
  size.labels <- 1
  box.size<-0.5
  n <- n_obs + n_proj

  def.par <- par(no.readonly = TRUE)

  #windows()

  # res <- 6
  # name_figure <- "Harvestrate.png"
  # png(filename = name_figure, height = 500*res, width = 650*res, res=72*res)

  par(mfrow = c(1,1), bty="n", mar=c(5,5,1,1))

  x = "h"
  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

  x.min = 1 ; x.max = n+1
  y.min = 0 ; y.max = 1.0
  line.y = y.max*2
  y.label = "Harvest Rate"
  x.label = "Years"
  title = "Harvest Rate (h=C/B)"

  boxplot(	X[,1],
           xaxt = "n", yaxt = "n",
           xlim = c(x.min,x.max), ylim = c(y.min,y.max),
           at = 1, outpch = NA, boxwex = box.size, col = 1)
  for (i in 2:n-1) {
    boxplot(X[,i], xaxt = "n", yaxt = "n", at = i, add = T, outpch = NA, boxwex = box.size, col = i) }

  points(x=x.min:(x.max-1),y=rep(line.y,n),col="red",type="l",lty=2,lwd="1")
  axis(side =1, at=1:n, labels = Year, las=3, cex.axis=size.labels)
  axis(side =2, cex.axis=size.labels)
  mtext(x.label, line = 3, side = 1, las=1, cex = size.text)
  mtext(y.label, line = 2.5, side = 2, cex = size.text)

  q_h_MSY <- quantile(mcmc.table$h_MSY,probs=c(0.05,0.5,0.95))
  abline(h=q_h_MSY[2], col = "green", lty=4, lwd = 3)
  abline(h=q_h_MSY[1], col = "green", lty=4, lwd = 1)
  abline(h=q_h_MSY[3], col = "green", lty=4, lwd = 1)

  par(def.par)
}
