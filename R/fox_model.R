#' this function develop the Fox global model from IA and Efox series
#'
#'
#' @param table_Efox  : table with IA and Efox
#' @param graph_param : vector gathering the graphhics aestetics parameters (format : c(lengthEfox, title, upper_x, upper_y, upper_ybis))
#' @param log         : by default, fit the model under log transformation of the AI. Else, fit without the log transformation (no start/limit)
#'
#' @examples
#'
#' @export



fox_model <- function(table_Efox, graph_param, a_start = 5, b_start= 3, log=TRUE){
  list_graph <- NULL

  #limits calculation
  upper_a <- max(table_Efox$IA, na.rm=T)*a_start
  lower_a <- max(table_Efox$IA, na.rm=T)*0.1
  start_a <- (upper_a + lower_a)/2
  upper_b <- max(table_Efox$Efox, na.rm=T) * b_start

  if (log==TRUE){
    modelefox_IA <- nls(formula = log(IA) ~ log(a)-b*Efox, data=table_Efox, start = c(a = start_a , b = upper_b/2), algorithm = "port", lower = c(a = lower_a, b = 0), upper = c(a=upper_a, b = upper_b))
  } else {
    modelefox_IA <- nls(formula = IA ~ a*exp(-b*Efox), data=table_Efox, start = c(a = start_a , b = upper_b/2))
  }

  x <- seq(0, graph_param[1], length = length(table_Efox$Efox))
  interval_confidence <- predictNLS(modelefox_IA, newdata=data.frame(Efox = x), interval="confidence", alpha=0.05, nsim=10000)$summary %>% mutate(Efox = x)
  interval_confidence_pred <- predictNLS(modelefox_IA, newdata=data.frame(Efox = table_Efox$Efox[2:length(table_Efox$Efox)]), interval="confidence", alpha=0.05, nsim=10000)$summary %>% mutate(Year = table_Efox$Year[2:length(table_Efox$Efox)])

  if (log ==T) {
    interval_confidence$`Sim.2.5%` <- exp(interval_confidence$`Sim.2.5%`)
    interval_confidence$`Sim.97.5%` <- exp(interval_confidence$`Sim.97.5%`)

    interval_confidence_pred$`Sim.2.5%` <- exp(interval_confidence_pred$`Sim.2.5%`)
    interval_confidence_pred$`Sim.97.5%` <- exp(interval_confidence_pred$`Sim.97.5%`)
  }

  par_Efox <- as.vector(coef(modelefox_IA))

  # Calcul valeurs MSY / calculating MSY values

  C_MSYfox <- (par_Efox[1]/(par_Efox[2]*exp(1)))*table_Efox$factEfox[1]
  E_MSYfox <- (1/par_Efox[2])
  IA_MSY <- par_Efox[1] * exp(-par_Efox[2] * E_MSYfox)

  # Un vecteur d'effort entre 0 et 12 par pas de 0.01 (pour les besoins du graphique, a adapter a votre etude)
  # A vector of effort values between 0 and 12, for each 0.01 (for the graphs, adapt it to your study)
  mE_fox <- seq(0,as.numeric(graph_param[1]),0.01)
  annee_etude <- as.numeric(table_Efox$Year[1]):as.numeric(table_Efox$Year[nrow(table_Efox)])

  #line_E <- par_E[1]*exp(-par_E[2]*mE_fox)

  IA_Efox <- par_Efox[1]*exp(-par_Efox[2]*mE_fox) # IA predits => tracer la courbe

  interval_confidence$prod_low <- interval_confidence$`Sim.2.5%` * x *table_Efox$factEfox[1]
  interval_confidence$prod_up <- interval_confidence$`Sim.97.5%` * x *table_Efox$factEfox[1]
  if (log ==T) {
    table_Efox$IA_pred <- c(NA, exp(predict(modelefox_IA))) # IA predits => tracer la courbe
  }
  else {
    table_Efox$IA_pred <- c(NA,predict(modelefox_IA)) # IA predits => tracer la courbe
  }

  Y_Efox <- IA_Efox * mE_fox * table_Efox$factEfox[1] # Y predits en multipliant IA par mE => tracer la courbe

  table_Efox2 <- table_Efox

  #plotFOX <- ggplot() + geom_line(aes(x=mE_fox, y=log_IA_Efox), color="blue")
  #plotFOX <- plotFOX + geom_point(aes(x=table_Efox$Efox, y=log(table_Efox$IA)), color="blue")
  #plotFOX <- plotFOX + ggtitle(paste(graph_param[2]))
  #plotFOX <- plotFOX + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  #print(plotFOX)
  data <- data.frame(mE_fox, IA_Efox, Y_Efox)
  #table_Efox$title <- graph_param[2]
  todo<-data.frame(title=c(rep(graph_param[2], 4),paste0(graph_param[2], "\n Abundance indices predicted vs observed")))
  #plotFOX_U <- plotFOX_U + ggtitle(paste(graph_param[2]))

  plotFOX_U <- table_Efox %>% mutate(title = todo[length(list_graph) + 1,]) %>% ggplot() +
    geom_point(aes(x=Efox, y=IA), color="black", size = 1) + geom_path(aes(x=Efox, y=IA)) +
    facet_grid(~title) +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=`Sim.2.5%`, ymax= `Sim.97.5%`), alpha = 0.15, inherit.aes=F, fill="blue") +
    geom_hline(yintercept= rep(table_Efox$IA[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +geom_vline(xintercept= rep(1,nrow(table_Efox)), linetype="dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Abundance indices") + geom_line(data = data, aes(x=mE_fox, y = IA_Efox), color="blue", size = 1.1)
  #plotFOX_U <- plotFOX_U + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  list_graph[[length(list_graph) + 1]] <- plotFOX_U


  plotFOX_U2 <- table_Efox %>% mutate(title = todo[length(list_graph) + 1,]) %>% ggplot() +
    geom_point(aes(x=Efox, y=IA), color="black", size = 1) +
    facet_grid(~title) +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=`Sim.2.5%`, ymax= `Sim.97.5%`), alpha=0.15, inherit.aes=F, fill="blue") +
    geom_line(data = data, aes(x=mE_fox, y = IA_Efox), color="blue", size = 1.1) +
    geom_hline(yintercept= rep(table_Efox$IA[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +geom_vline(xintercept= rep(1,nrow(table_Efox)), linetype="dashed", color = "red") +
    geom_path(aes(x=Efox, y=IA), linetype="twodash", size = 1) +
    geom_text(aes(x=table_Efox$Efox, y=table_Efox$IA, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=4) +
    theme_nice() + labs(x = "mE", y = "Abundance indices")
  list_graph[[length(list_graph) + 1]] <- plotFOX_U2


  plotFOX_Y <- table_Efox %>% mutate(title = todo[length(list_graph) + 1,]) %>% ggplot() +
    geom_point(aes(x=E, y=Capture), color="black", size = 1.3)+ facet_grid(~title) + labs(x = "mE") +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=prod_low, ymax= prod_up), alpha = 0.15, inherit.aes=F, fill="blue") +
    geom_hline(yintercept= rep(table_Efox$Capture[nrow(table_Efox)], nrow(table_Efox)), linetype="dashed", color = "red") + geom_vline(xintercept= rep(table_Efox$E[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Catch") + geom_line(data = data, aes(x=mE_fox, y = Y_Efox), color="blue", size = 1.1) # D.G a dit de prendre E
  list_graph[[length(list_graph) + 1]] <- plotFOX_Y


  plotFOX_Y2 <- table_Efox %>% mutate(title = todo[length(list_graph) + 1,]) %>% ggplot() +
    geom_point(aes(x=E, y=Capture), color="black", size = 1) +
    facet_grid(~title) +
    geom_path(aes(x=E, y=Capture), linetype="twodash", size = 1.1) +
    geom_text(aes(x=E, y=Capture, label=stringi::stri_sub(Year,3,4)), hjust=-0.5, vjust=0, size=4) +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=prod_low, ymax= prod_up), alpha = 0.15, inherit.aes=F, fill="blue") +
    geom_hline(yintercept= rep(table_Efox$Capture[nrow(table_Efox)], nrow(table_Efox)), linetype="dashed", color = "red") +
    geom_vline(xintercept= rep(table_Efox$E[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +
    labs(x = "mE", y = "Catch") + theme_nice() + geom_line(data = data, aes(x=mE_fox, y = Y_Efox), color="blue", size = 1.1)
  list_graph[[length(list_graph) + 1]] <- plotFOX_Y2


  plotFOX_pred <- table_Efox %>%  full_join(interval_confidence_pred[,c(11:13)], by  = 'Year') %>%
    mutate(title = todo[length(list_graph) + 1,]) %>% ggplot() +
    geom_line(aes(x=Year, y=IA_pred), color="black") +
    geom_point(aes(x=Year, y=IA), color="black") +  #Efox
    geom_ribbon(aes(x=Year, ymin=`Sim.2.5%`, ymax= `Sim.97.5%`), alpha = 0.15, inherit.aes=F, fill="#333232") +
    facet_grid(~title) + theme_nice() + labs(x = "Year", y = "Abundance indices" )
  #plotFOX_pred <- plotFOX_pred + xlim(table_Efox$Year[1], table_Efox$Year[nrow(table_Efox)]) + ylim(0,as.numeric(graph_param[4]))
  list_graph[[length(list_graph) + 1]] <- plotFOX_pred
  plot(nlsResiduals(modelefox_IA))
if (log == T) {AIC <- AIC(modelefox_IA) + 2*sum(log(table_Efox$IA))}
else {AIC <- AIC(modelefox_IA)}
  names_values <- c("a", "b", "m", "MSY", "E/E_msy", "B/BMSY",
                    "B/B0", "AIC")

  results <- round(c(par_Efox[1],
                     par_Efox[2],
                     par_Efox[3],
                     C_MSYfox,
                     tail(table_Efox$Efox, 1)/E_MSYfox,
                     tail(table_Efox$IA,1)/IA_MSY,
                     tail(table_Efox$IA, 1)/IA_Efox[1],
                     AIC(modelegene_IA)),2)

  table_outputs <- rbind(names_values, results)

  print(table_outputs)

  return(list(table_outputs, list_graph))

}

