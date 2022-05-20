#'  Bayesian global model, on encapsule un modèle global générique dans la fonction
#' @param mcmc mcmc liste venant de la fonction bgm

#' @examples
#'

#' @export

bgm_output_5<- function (mcmc,data,n_proj=0,multiple=FALSE, title){

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
    dplyr::summarize(Mean_C = mean(value, na.rm=TRUE),
                     lower = quantile(value,probs = c(0.05)),
                     upper = quantile(value,probs = c(0.95)),
                     lower2 = quantile(value,probs = c(0.25)),
                     upper2 = quantile(value,probs = c(0.75)))->X1

 data$title <- paste0("Fit Catches \n", title)
 g<- ggplot(X1)+
    geom_ribbon(aes(ymin=lower, ymax=upper, x=Year,group=1,fill="CI 95 %"))+
    geom_ribbon(aes(ymin=lower2, ymax=upper2, x=Year,group=1,fill="CI 75 %"))+
    scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray85", "CI 75 %" = "gray70"))+
    geom_line(aes(x=Year,y=Mean_C,group="Mean",color='Mean'))+
    geom_point(data=data, aes(x=factor(Year),y=C,color='Cobs'))+
    scale_color_manual(name='',  values=c("Cobs" = "black", "Mean" = 'red'))+
    ylab("Catches")+ xlab("Year") + theme_nice() +   theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
    facet_grid(~title)


print(g)

  # dev.off()

  # ----------------------------------------------
  # Abundance indices - predicted vs observed
  # ----------------------------------------------

  def.par <- par(no.readonly = TRUE)

  #windows()

  par(mfrow = c(1,1), mar=c(5,5,1,1))

  col = c("lightgray","gray")
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

  #x = "I_pred"
  x = "I"

  #mcmc <- window(mcmc)
  mcmc.table <- as.data.frame(as.matrix(mcmc))
  X = mcmc.table[,which(substr(colnames(mcmc.table),1,nchar(x)+1)==paste(x,"[",sep=""))]
  names(X)<-data$Year[seq(1,length(names(X)))]



  X %>%pivot_longer(cols=names(X), names_to = "Year") %>% group_by(Year) %>%
    dplyr::summarize(Mean_C = mean(value, na.rm=TRUE),
                     lower = quantile(value,probs = c(0.05)),
                     upper = quantile(value,probs = c(0.95)),
                     lower2 = quantile(value,probs = c(0.25)),
                     upper2 = quantile(value,probs = c(0.75)))->X2

  data$title <- paste0("Fit AI \n", title)
  g<- ggplot(X2)+
    geom_ribbon(aes(ymin=lower, ymax=upper, x=Year,group=1,fill="CI 95 %"))+
    geom_ribbon(aes(ymin=lower2, ymax=upper2, x=Year,group=1,fill="CI 75 %"))+
    scale_fill_manual(name='CI',  values=c("CI 95 %" = "gray85", "CI 75 %" = "gray70"))+
    geom_line(aes(x=Year,y=Mean_C,group="Mean",color='Mean'))+
    geom_point(data=data, aes(x=factor(Year),y=I,color='Iobs'))+
    scale_color_manual(name='',  values=c("Iobs" = "black", "Mean" = 'red'))+
    ylab("Abundance index")+ xlab("Year") + theme_nice() +   theme(axis.text.x = element_text(angle = 90)) +
    theme(legend.position = c(0.95, 0.95), legend.justification = c("right", "top")) +
    facet_grid(~title)


  print(g)


}
