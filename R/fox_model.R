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



fox_model <- function(table_Efox, graph_param, a_start = 5, b_start= 3, logarithmic=TRUE){

  #limits calculation
  upper_a <- max(table_Efox$IA, na.rm=T)*a_start
  lower_a <- max(table_Efox$IA, na.rm=T)*0.1
  start_a <- (upper_a + lower_a)/2
  upper_b <- max(table_Efox$Efox, na.rm=T) * b_start

  if (logarithmic==TRUE){
    modelefox_IA <- nls(formula = log(IA) ~ log(a)-b*Efox, data=table_Efox, start = c(a = start_a , b = upper_b/2), algorithm = "port", lower = c(a = lower_a, b = 0), upper = c(a=upper_a, b = upper_b))
  } else {
    modelefox_IA <- nls(formula = IA ~ a*exp(-b*Efox), data=table_Efox, start = c(a = start_a , b = upper_b/2))
  }

  par_Efox <- as.vector(coef(modelefox_IA))

  # Calcul valeurs MSY / calculating MSY values

  C_MSYfox <- (par_Efox[1]/(par_Efox[2]*exp(1)))*table_Efox$factEfox[1]
  E_MSYfox <- (1/par_Efox[2])

  # Un vecteur d'effort entre 0 et 12 par pas de 0.01 (pour les besoins du graphique, a adapter a votre etude)
  # A vector of effort values between 0 and 12, for each 0.01 (for the graphs, adapt it to your study)
  mE_fox <- seq(0,as.numeric(graph_param[1]),0.01)
  annee_etude <- as.numeric(table_Efox$Year[1]):as.numeric(table_Efox$Year[nrow(table_Efox)])

  #line_E <- par_E[1]*exp(-par_E[2]*mE_fox)
  IA_Efox <- par_Efox[1]*exp(-par_Efox[2]*mE_fox)
  log_IA_Efox <- log(par_Efox[1]) - par_Efox[2]*mE_fox
  Y_Efox <- IA_Efox * mE_fox *table_Efox$factEfox[1]
  table_Efox$IA_pred <- par_Efox[1]*exp(-par_Efox[2]*table_Efox$Efox)
  table_Efox2 <- table_Efox

  #plotFOX <- ggplot() + geom_line(aes(x=mE_fox, y=log_IA_Efox), color="blue")
  #plotFOX <- plotFOX + geom_point(aes(x=table_Efox$Efox, y=log(table_Efox$IA)), color="blue")
  #plotFOX <- plotFOX + ggtitle(paste(graph_param[2]))
  #plotFOX <- plotFOX + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  #print(plotFOX)

  table_Efox$title <- graph_param[2]
  plotFOX_log <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="red", size = 1.1)
  #plotFOX_log <- plotFOX_log + ggtitle(paste(graph_param[2]))
  plotFOX_log <- plotFOX_log +
    geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black", size = 1) +
    geom_path() +
    facet_grid(~table_Efox$title) +
    theme_nice()
  #plotFOX_log <- plotFOX_log + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  print(plotFOX_log)


  plotFOX_log_test <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="red", size = 1.1)
  plotFOX_log_test <- plotFOX_log_test +
    geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black", size = 1) +
    geom_path(aes(x=table_Efox$Efox, y=table_Efox$IA), linetype="twodash", size = 1)
  plotFOX_log_test <- plotFOX_log_test +
    geom_text(aes(x=table_Efox$Efox, y=table_Efox$IA, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=3) +
    facet_grid(~table_Efox$title) +
    theme_nice()
  #plotFOX_log_test <- plotFOX_log_test + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))

  print(plotFOX_log_test)

  #?????
  plotFOX2 <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="red", size = 1.1)
  plotFOX2 <- plotFOX2 + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black", size = 1.3)+ facet_grid(~table_Efox$title) + labs(x = "mE") + theme_nice() #E ou Efox? D.a dit de prendre E je crois
  #plotFOX2 <- plotFOX2 + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  print(plotFOX2)

  plotFOX2_test <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="red", size = 1.1)
  #plotFOX2_test <- plotFOX2_test + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  plotFOX2_test <- plotFOX2_test + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black", size = 1) + geom_path(aes(x=table_Efox$E, y=table_Efox$Capture), linetype="twodash", size = 1) #E ou Efox? D.a dit de prendre E je crois
  plotFOX2_test <- plotFOX2_test + geom_text(aes(x=table_Efox$E, y=table_Efox$Capture, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=3) + facet_grid(~table_Efox$title) + labs(x = "mE") + theme_nice()
  print(plotFOX2_test)



  #???
  plotFOX3 <- ggplot() + geom_line(aes(x=table_Efox$Year, y=table_Efox$IA_pred), color="black")
  plotFOX3 <- plotFOX3 + geom_point(aes(x=table_Efox$Year, y=table_Efox$IA), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotFOX3 <- plotFOX3  + facet_grid(~table_Efox$title) + theme_nice()
  #plotFOX3 <- plotFOX3 + xlim(table_Efox$Year[1], table_Efox$Year[nrow(table_Efox)]) + ylim(0,as.numeric(graph_param[4]))
  print(plotFOX3)



  names_values <- c("Coeff_a", "Coeff_b", "MSY_recalcule", "Emsy_recalcule")
  results <- c(par_Efox[1], par_Efox[2], C_MSYfox, E_MSYfox)
  table_outputs <- rbind(names_values, results)

  print(table_outputs)

  return(table_outputs)

}

