#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_5<- function (mcmc,data,n_proj=0){

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
  if (n_proj>0) {Year = c(Year, seq(from = max(Year), to = max(Year)+n_proj-1, by=1))}
  #windows()

  par(mfrow = c(1,1), mar=c(5,5,1,1))

  col = c("lightgray","gray")

  # x = "C_pred"
  x = "C"

  #mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  names(X)<-data$Year[seq(1,length(names(X)))]



  X %>%pivot_longer(cols=names(X), names_to = "Year") %>% group_by(Year) %>%
    dplyr::summarize(Mean_C = mean(value, na.rm=TRUE),lower = quantile(value,probs = c(0.05)),upper = quantile(value,probs = c(0.95)),lower2 = quantile(value,probs = c(0.25)),upper2 = quantile(value,probs = c(0.75)))->X1


 g<- ggplot(X1)+
    geom_ribbon(aes(ymin=lower, ymax=upper, x=Year,group=1,fill="C(0.5,0.95)"))+
    geom_ribbon(aes(ymin=lower2, ymax=upper2, x=Year,group=1,fill="C(0.25,0.75)"))+
    scale_fill_manual(name='',  values=c("C(0.5,0.95)" = "grey70", "C(0.25,0.75)" = "grey"))+
    geom_line(aes(x=Year,y=Mean_C,group="Mean",color='Mean'))+
    geom_point(data=complet,aes(x=factor(Year),y=C,color='Cobs'))+
    ylab("Catches")+
    theme(axis.text.x = element_text(angle=90))+
    ggtitle("Fit Catches")


print(g)

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
