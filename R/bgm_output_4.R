#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_4<- function (mcmc,data,n_proj=4,multiple=FALSE, title){

  # ----------------------------------------------------------------------------
  # Equilibrium curve
  # ----------------------------------------------------------------------------
  par(mfrow = c(1,1))
  #windows()
  size.text <- 1.3
  size.labels <- 1
  box.size<-0.5
  x = "C_e"
  B_e <- seq(0, max_c/5, length = 50)
  mE <- seq(0, max(tab_input$E, na.rm = T)+1, length = 50)
  n_equi <- length(B_e)

  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]

  col = c("lightgray","gray")

  C.med <- apply(X,2,median)

  # IC 95 %
  C.inf95 <- NULL
  C.sup95 <- NULL
  for (k in 1:dim(X)[2])
  {
    C.inf95[k] <- quantile(X[,k],probs=0.05)
    C.sup95[k] <- quantile(X[,k],probs=0.95)
  }

  # IC 75%
  C.inf75 <- NULL
  C.sup75 <- NULL
  for (k in 1:dim(X)[2])
  {
    C.inf75[k] <- quantile(X[,k],probs=0.25)
    C.sup75[k] <- quantile(X[,k],probs=0.75)
  }

  tab <- data.frame(title = rep(paste0("Catch evolution as function of biomass \n", title),50))
  q_C_MSY <- quantile(mcmc.table$C_MSY,probs=c(0.05,0.5,0.95))
  q_B_MSY <- quantile(mcmc.table$B_MSY,probs=c(0.05,0.5,0.95))

  x = "B"
  mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  B.med <- apply(X,2,median)

  Year <- data$Year[seq(1,length(names(X)))]

  # ggplot() +
  #   geom_hline(yintercept= rep(q_C_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_vline(xintercept= rep(q_B_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_hline(yintercept= rep(q_C_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_vline(xintercept= rep(q_B_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_path(aes(x=B.med, y=data$C[1:length(B.med)]), linetype="solid") +
  #   geom_ribbon(aes(ymin=C.inf95, ymax=C.sup95, x=B_e,group=1,fill="CI 95 %"), alpha = 0.55)+
  #   geom_ribbon(aes(ymin=C.inf75, ymax=C.sup75, x=B_e,group=1,fill="CI 75 %"), alpha = 0.65)+
  #   scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray80", "CI 75 %" = "gray70")) +
  #   geom_hline(yintercept= rep(q_C_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
  #   geom_vline(xintercept= rep(q_B_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
  #   geom_line(aes(x=B_e, y= C.med), color="red", size = 1.1) +
  #   geom_point(aes(x=B.med, y=data$C[1:length(B.med)]), pch = 0) +  facet_grid(~tab$title) +
  #   theme_nice() + labs(x = "Biomass", y = "Catch")  + theme(legend.position = c(0.95, 0.95),
  # legend.justification = c("right", "top")) +
  #   coord_cartesian(ylim = c(0, max(data$C[1:length(B.med)])*1.1))

   Ce_plot <- ggplot() +
    geom_hline(yintercept= rep(q_C_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
    geom_vline(xintercept= rep(q_B_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
    geom_hline(yintercept= rep(q_C_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
    geom_vline(xintercept= rep(q_B_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
    geom_path(aes(x=B.med, y=data$C[1:length(B.med)]), linetype="solid") +
    geom_ribbon(aes(ymin=C.inf95, ymax=C.sup95, x=B_e,group=1,fill="CI 95 %"), alpha = 0.55)+
    geom_ribbon(aes(ymin=C.inf75, ymax=C.sup75, x=B_e,group=1,fill="CI 75 %"), alpha = 0.65)+
    scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray80", "CI 75 %" = "gray70")) +
    geom_hline(yintercept= rep(q_C_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
    geom_vline(xintercept= rep(q_B_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
    geom_line(aes(x=B_e, y= C.med), color="red", size = 1.1) +
    geom_point(aes(x=B.med, y=data$C[1:length(B.med)]), pch = 0) +
    geom_text(aes(x=B.med, y=data$C[1:length(B.med)], label=stringi::stri_sub(Year,3,4)), hjust=-0.5, vjust=0, size=4) +  facet_grid(~tab$title) +
    theme_nice() + labs(x = "Biomass", y = "Catches") +
    theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
    coord_cartesian(ylim = c(0, max(data$C[1:length(B.med)])*1.1))

   plot(Ce_plot)

#-------------------------
#
# Y_e
#
#-------------------------

  # x = "Y_e"
  # mE <- seq(0, max(data$E)+1, length = 50)
  # n_equi <- length(B_e)
  #
  # mcmc <- window(mcmc)
  # mcmc.table <- as.data.frame(as.matrix(mcmc))
  # X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  #
  # col = c("lightgray","gray")
  #
  # Y.med <- apply(X,2,median)
  #
  # # IC 95 %
  # Y.inf95 <- NULL
  # Y.sup95 <- NULL
  # for (k in 1:dim(X)[2])
  # {
  #   Y.inf95[k] <- quantile(X[,k],probs=0.05)
  #   Y.sup95[k] <- quantile(X[,k],probs=0.95)
  # }
  #
  # # IC 75%
  # Y.inf75 <- NULL
  # Y.sup75 <- NULL
  # for (k in 1:dim(X)[2])
  # {
  #   Y.inf75[k] <- quantile(X[,k],probs=0.25)
  #   Y.sup75[k] <- quantile(X[,k],probs=0.75)
  # }
  #
  # tab <- data.frame(title = rep(paste0("Catch evolution as function of biomass \n", title),50))
  # q_Y_MSY <- quantile(mcmc.table$C_MSY,probs=c(0.05,0.5,0.95))
  # q_E_MSY <- quantile(mcmc.table$E_MSY,probs=c(0.05,0.5,0.95))


  E <- data$E
  Ye_mE <- data.frame(ce= C.med, mE =mE)
  # ggplot() +
  #   geom_hline(yintercept= rep(q_C_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_vline(xintercept= rep(q_B_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_hline(yintercept= rep(q_C_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_vline(xintercept= rep(q_B_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
  #   geom_path(aes(x=B.med, y=data$C[1:length(B.med)]), linetype="solid") +
  #   geom_ribbon(aes(ymin=C.inf95, ymax=C.sup95, x=B_e,group=1,fill="CI 95 %"), alpha = 0.55)+
  #   geom_ribbon(aes(ymin=C.inf75, ymax=C.sup75, x=B_e,group=1,fill="CI 75 %"), alpha = 0.65)+
  #   scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray80", "CI 75 %" = "gray70")) +
  #   geom_hline(yintercept= rep(q_C_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
  #   geom_vline(xintercept= rep(q_B_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
  #   geom_line(aes(x=B_e, y= C.med), color="red", size = 1.1) +
  #   geom_point(aes(x=B.med, y=data$C[1:length(B.med)]), pch = 0) +  facet_grid(~tab$title) +
  #   theme_nice() + labs(x = "Biomass", y = "Catch")  + theme(legend.position = c(0.95, 0.95),
  # legend.justification = c("right", "top")) +
  #   coord_cartesian(ylim = c(0, max(data$C[1:length(B.med)])*1.1))

  Ye_plot <- ggplot() +
    geom_hline(yintercept= rep(q_C_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
    #geom_vline(xintercept= rep(q_E_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
    geom_hline(yintercept= rep(q_C_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
    #geom_vline(xintercept= rep(q_E_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
    geom_path(aes(x=E, y=data$C[1:length(E)]), linetype="solid") +
    geom_ribbon(aes(ymin=C.inf95, ymax=C.sup95, x=mE, group=1,fill="CI 95 %"), alpha = 0.55)+
    geom_ribbon(aes(ymin=C.inf75, ymax=C.sup75, x=mE, group=1,fill="CI 75 %"), alpha = 0.65)+
    scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray80", "CI 75 %" = "gray70")) +
    geom_hline(yintercept= rep(q_C_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
    geom_vline(xintercept= rep(mE[match(max(round(Ye_mE$ce)), round(Ye_mE$ce))],nrow(tab)), linetype="dashed", color = "blue") +
    geom_line(aes(x=mE, y= C.med), color="red", size = 1.1) +
    geom_point(aes(x=E, y=data$C[1:length(E)]), pch = 0) +
    geom_text(aes(x=E, y=data$C[1:length(E)], label=stringi::stri_sub(Year,3,4)), hjust=-0.5, vjust=0, size=4) +  facet_grid(~tab$title) +
    theme_nice() + labs(x = "Effort multiplier", y = "Catches") +
    theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
    coord_cartesian(ylim = c(0, max(data$C[1:length(E)])*1.1))

   plot(Ye_plot)

   # if(q_E_MSY[1]>10) {
   #
   # Ye_plot_E <- ggplot() +
   #   geom_hline(yintercept= rep(q_C_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
   #   geom_vline(xintercept= rep(q_E_MSY[1],nrow(tab)), linetype="dotted", color = "blue") +
   #   geom_hline(yintercept= rep(q_C_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
   #   #geom_vline(xintercept= rep(q_E_MSY[3],nrow(tab)), linetype="dotted", color = "blue") +
   #   geom_path(aes(x=E, y=data$C[1:length(E)]), linetype="solid") +
   #   geom_ribbon(aes(ymin=Y.inf95, ymax=Y.sup95, x=mE, group=1,fill="CI 95 %"), alpha = 0.55)+
   #   geom_ribbon(aes(ymin=Y.inf75, ymax=Y.sup75, x=mE, group=1,fill="CI 75 %"), alpha = 0.65)+
   #   scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray80", "CI 75 %" = "gray70")) +
   #   #geom_hline(yintercept= rep(q_C_MSY[2],nrow(tab)), linetype="dashed", color = "blue") +
   #   #geom_vline(xintercept= rep(data[match(q_C_MSY[1], data$C)],nrow(tab)), linetype="dashed", color = "blue") +
   #
   #   geom_line(aes(x=mE, y= Y.med), color="red", size = 1.1) +
   #   #geom_point(aes(x=E, y=data$C[1:length(E)]), pch = 0) +
   #   geom_text(aes(x=E, y=data$C[1:length(E)], label=stringi::stri_sub(Year,3,4)), hjust=-0.5, vjust=0, size=4) +  facet_grid(~tab$title) +
   #   theme_nice() + labs(x = "Effort multiplier", y = "Catches") +
   #   theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
   #   coord_cartesian(ylim = c(0, max(data$C[1:length(E)])*1.1))
   #
   # plot(Ye_plot_E)
   # }
   print(paste0("E_MSY = ", round(mE[match(max(round(Ye_mE$ce)), round(Ye_mE$ce))], digits = 2)))
   print(paste0("C_MSY = ", round(q_C_MSY[2]), " T"))
}
