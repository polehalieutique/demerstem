#' Cette fonction réalise le modele de Fox à partir d'un IA et d'Efox
#'
#'
#' @param data : le tableau avec IA
#' @param table_capture : table avec les captures et les années (format :
#' @param k : facteur k du calcul de l'effort de Fox. Au choix : 1, 3 ou 5
#'
#' @examples
#'
#' @export



modelo_fox <- function(table_Efox, upper_b, param_graph){

  #limits calculation
  upper_a <- max(table_Efox$IA, na.rm=T)*3 #*3? #Efox?!
  lower_a <- max(table_Efox$IA, na.rm=T)*0.5
  start_a <- (upper_a + lower_a)/2

  modelefox_IA <- nls(formula = log(IA) ~ log(a)-b*Efox, data=table_Efox, start = c(a = start_a , b = upper_b/2), algorithm = "port", lower = c(a = lower_a, b = 0), upper = c(a=upper_a, b = upper_b))
  #modelefox_IA <- nls(formula = IA_MOYEN_STANDARD ~ a*exp(-b*Efox), data=data_IA_FINAL, start = c(a = 4.44 , b = 0.6), algorithm = "default")
  par_Efox <- as.vector(coef(modelefox_IA))

  #modelefox_IA_Efox <- nls(formula = log(IA_MOYEN_STANDARD) ~ log(a)-b*Efox, data=data_IA_FINAL, start = coef(modelefox_IA), algorithm = "port", lower = c(a = 1, b = 0), upper = c(a=10, b = 3))
  #modelefox_IA_Efox <- nls(formula = IA_MOYEN_STANDARD ~ a*exp(-b*Efox), data=data_IA_FINAL, start = coef(modelefox_IA), algorithm = "default")
  #par_Efox <- as.vector(coef(modelefox_IA_Efox))


  # Calcul valeurs MSY / calculating MSY values

  C_MSYfox <- (par_Efox[1]/(par_Efox[2]*exp(1)))*table_Efox$factEfox[1]
  E_MSYfox <- (1/par_Efox[2])

  # Intervalles de confiance / confidence interval
  # ------------------------

  #confint(modelefox_IA, level = 0.5)
  #confint(modelefox_IA_Efox, level = 0.5)



  # Un vecteur d'effort entre 0 et 12 par pas de 0.01 (pour les besoins du graphique, a adapter a votre etude)
  # A vector of effort values between 0 and 12, for each 0.01 (for the graphs, adapt it to your study)
  mE_fox <- seq(0,as.numeric(param_graph[1]),0.01)
  annee_etude <- as.numeric(table_Efox$Year[1]):as.numeric(table_Efox$Year[nrow(table_Efox)])

  #line_E <- par_E[1]*exp(-par_E[2]*mE_fox)
  IA_Efox <- par_Efox[1]*exp(-par_Efox[2]*mE_fox)
  Y_Efox <- IA_Efox * mE_fox *table_Efox$factEfox[1]
  table_Efox$IA_pred <- par_Efox[1]*exp(-par_Efox[2]*table_Efox$Efox)



  plotFOX <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="black")
  plotFOX <- plotFOX + geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black")
  plotFOX <- plotFOX + ggtitle(paste(param_graph[2]))
  plotFOX <- plotFOX + xlim(0,as.numeric(param_graph[3])) + ylim(0,as.numeric(param_graph[4]))
  print(plotFOX)

  #?????
  plotFOX2 <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="black")
  plotFOX2 <- plotFOX2 + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotFOX2 <- plotFOX2 + ggtitle(paste(param_graph[2]))
  plotFOX2 <- plotFOX2 + xlim(0,as.numeric(param_graph[3])) + ylim(0,as.numeric(param_graph[5]))
  print(plotFOX2)

  #???
  plotFOX3 <- ggplot() + geom_line(aes(x=table_Efox$Year, y=table_Efox$IA_pred), color="black")
  plotFOX3 <- plotFOX3 + geom_point(aes(x=table_Efox$Year, y=table_Efox$IA), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotFOX3 <- plotFOX3 + ggtitle(paste(param_graph[2]))
  plotFOX3 <- plotFOX3 + xlim(table_Efox$Year[1], table_Efox$Year[nrow(table_Efox)]) + ylim(0,as.numeric(param_graph[4]))
  print(plotFOX3)



  names_values <- c("Coeff_a", "Coeff_b", "MSY_recalcule", "Emsy_recalcule")
  results <- c(par_Efox[1], par_Efox[2], C_MSYfox, E_MSYfox)
  table_outputs <- rbind(names_values, results)

  print(table_outputs)

  return(table_outputs)

}

