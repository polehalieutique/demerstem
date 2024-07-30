#' Bayesian function (not used for the moment)
#' \code{bgm_output_5} fonction graphique
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export


bgm_output_2<- function (mcmc,data,n_proj=0,multiple=FALSE){

  # ------------------------------------------------------------------------------------
  # Customized nice graphs
  # ------------------------------------------------------------------------------------

  # ---------------------------------------------------
  # Joint and marginal posterior distributions
  # ---------------------------------------------------
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  x1 <- mcmc.table[,'K']
  x2 <- mcmc.table[,'r']
  x3 <- mcmc.table[,'C_MSY']
  if (multiple)
    {
    x4_1 <- mcmc.table[,'q1']
    x4_2 <- mcmc.table[,'q2']
    x4_3 <- mcmc.table[,'q3']
  }
  else
  {
    x4 <- mcmc.table[,'q']
  }
  x5 <- mcmc.table[,'sigma2p']

  def.par <- par(no.readonly = TRUE)
  #windows()

  par(pch='.')

  if (multiple)
    {
    pairs( cbind(x1,x2,x3,x4_1,x4_2,x4_3,x5),labels=c("K","r",expression(C[MSY]),"q1","q2","q3",expression(sigma[p]^2)),
         lower.panel=panel.smooth,
         diag.panel=panel.dens,
         upper.panel=panel.cor,
         cex.labels = 2.0, font.labels=1.2)
  }
  else
  {
    pairs( cbind(x1,x2,x3,x4,x5),labels=c("K","r",expression(C[MSY]),"q",expression(sigma[p]^2)),
           lower.panel=panel.smooth,
           diag.panel=panel.dens,
           upper.panel=panel.cor,
           cex.labels = 2.0, font.labels=1.2)
  }



  # ---------------------------------------------------
  # 2D posterior distribution with contours
  # ---------------------------------------------------


  dim1 <- list(mcmc.table[,'K'], var_name = "K")
  dim2 <- list(mcmc.table[,'r'], var_name = "r")

  def.par <- par(no.readonly = TRUE)
  #windows()
  f.density.bivar(dim1,dim2,nlevels=3,nb.points=2000)
  par(def.par)




  # --------------------------------------------------------------------------
  # Nice graphs - Boxplot of time series
  # --------------------------------------------------------------------------
  n_obs <- length(data$Year)

  n <- n_obs + n_proj
  if (multiple)
    {
    I_obs1 <- data$I_obs1
    I_obs2 <- data$I_obs2
    I_obs3 <- data$I_obs3
  }
  else
  {
    I_obs <- data$I_obs
  }
  Year = data$Year
  if (n_proj>0) { Year = c(Year, seq(from = max(Year), to = max(Year)+n_proj-1, by=1))}

  size.text <- 1.3
  size.labels <- 1
  box.size <- 0.5
  col.obs <- "white"
  # col.proj <- "grey"
  col.proj <- "grey75"
  # col2 <- "darkgrey"
  col2 <- "grey30"
  col <- c(rep(col.obs, times=n_obs),rep(col.proj, times=n_proj))
  col2 <- rep(col2, times=n)


  # ----------------------------------------------------------------------------
  # Marginal posterior distribution - MSY
  # ----------------------------------------------------------------------------

  def.par <- par(no.readonly = TRUE)

  #windows()

  # res <- 6
  # name_figure <- "PostMSY.png"
  # png(filename = name_figure, height = 500*res, width = 500*res, res=72*res)

  par(mar=c(5,5,1,1), bty="n")

  densMSY <- density(mcmc.table$C_MSY)
  densMSYp <- density(mcmc.table$C_MSY_p)
  plot(densMSY ,ylab = "", xlab = "", xaxt="n", yaxt="n", main="", xlim = c(0,max(densMSY$x)), col = "black", type = "l", lty = 1, lwd = 2)
  points(densMSYp ,ylab = "", xlab = "", xaxt="n", yaxt="n", main="", xlim = c(0,max(densMSY$x)), col = "black", type = "l", lty = 2, lwd = 2)

  axis(side = 1, tick = T, cex.axis=1, las=1)
  axis(side = 2, tick = T, lwd.ticks=0, labels=NA, cex.axis=1)
  mtext(side=1, "MSY (x 1000 tons)", bty="n", line=3, cex = size.text)
  mtext(side=2, "", bty="n", line=3, cex = size.text)

  legend(	legend = c("Prior","Posterior"),
          col = c("black","black"),
          lty = c(2,1), lwd = c(2,2), x = "topright", cex = size.text, bty ="n")

  par(def.par)

  # dev.off()


  # ----------------------------------------------
  # Biomass
  # ----------------------------------------------

  def.par <- par(no.readonly = TRUE)

  #windows()

  # res <- 6
  # name_figure <- "Biomass.png"
  # png(filename = name_figure, height = 500*res, width = 650*res, res=72*res)

  par(mfrow = c(1,1), bty="n", mar=c(5,5,1,1))

  x = "B"
  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  x.min = 1 ; x.max = n+1
  y.min = 0 ; y.max = max(X)
  line.y = y.max*2
  y.label = "Biomass"
  x.label = "Years"
  title = "Biomass (X 1000 tons)"

  boxplot(	X[,1],
           xaxt = "n", yaxt = "n",
           xlim = c(x.min,x.max), ylim = c(y.min,y.max),
           at = 1, outpch = NA, boxwex = box.size, col = col[1])
  for (i in 2:n) {
    boxplot(X[,i], xaxt = "n", yaxt = "n", at = i, add = T, outpch = NA, boxwex = box.size, col = col[i]) }

  points(x=x.min:(x.max-1),y=rep(line.y,n),col="red",type="l",lty=2,lwd="1")
  axis(side =1, at=1:n, labels = Year, las=3, cex.axis=size.labels)
  axis(side =2, cex.axis=size.labels)
  mtext(x.label, line = 3, side = 1, las=1, cex = size.text)
  mtext(y.label, line = 2, side = 2, cex = size.text)

  q_B_MSY <- quantile(mcmc.table$B_MSY,probs=c(0.05,0.5,0.95))
  abline(h=q_B_MSY[2], col = "gray10", lty=4, lwd = 3)
  abline(h=q_B_MSY[1], col = "gray30", lty=4, lwd = 1)
  abline(h=q_B_MSY[3], col = "gray30", lty=4, lwd = 1)

  par(def.par)
}
