#' this function develop the generalised production model from IA and Efox series
#'
#'
#' @param table_Efox      : table with IA and Efox
#' @param graph_param     : vector gathering the graphhics aestetics parameters (format : c(lengthEfox, title, upper_x, upper_y, upper_ybis))
#' @param log             : by default, fit the model under log transformation of the AI. Else, fit without the log transformation (no start/limit)
#' @param vect_ini        : values of initialisation of the parameters a, b and p
#' @param vect_lower      : values of lower limits of the parameters a, b and p
#' @param vect_upper      : values of upper limits of the parameters a, b and p
#' @param warning_control : if TRUE, the nls won't stop its process if too many warnings are made.
#' @param max_iteration   : number of iteration that will be runned.
#'
#' @examples
#'
#' @export


generalised_model <- function(table_Efox, graph_param, vect_ini=c(10, -0.5, 1), vect_lower=c(0.001, -1, 0.01), vect_upper=c(100, 10, 9), warning_control = FALSE, max_iteration = 50){

  upper_a <- max(table_Efox$IA, na.rm=T)*5
  lower_a <- max(table_Efox$IA, na.rm=T)*0.1
  start_a <- (upper_a + lower_a)/2
  upper_b <- max(table_Efox$Efox, na.rm=T) * 3


  if (warning_control==TRUE){
    modelefox_IA <- nls(formula = IA ~ (a+b*Efox)^(1/p), data=table_Efox, start = c(a = vect_ini[1] , b = vect_ini[2], p=vect_ini[3]), lower = c(a = vect_lower[1] , b=vect_lower[2], p = vect_lower[3]), upper = c(a = vect_upper[1], b=vect_upper[2], p = vect_upper[3]), algorithm = "port", nls.control(warnOnly = TRUE, maxiter = max_iteration))
  } else {
    modelefox_IA <- nls(formula = IA ~ (a+b*Efox)^(1/p), data=table_Efox, start = c(a = vect_ini[1] , b = vect_ini[2], p=vect_ini[3]), lower = c(a = vect_lower[1] , b=vect_lower[2], p = vect_lower[3]), upper = c(a = vect_upper[1], b=vect_upper[2], p = vect_upper[3]), algorithm = "port", nls.control(maxiter = max_iteration))
  }


  par_Efox <- as.vector(coef(modelefox_IA))
  C_MSY <- -(par_Efox[3]/par_Efox[2])*((par_Efox[1]/(1+par_Efox[3]))^(1+(1/par_Efox[3])))*table_Efox$factEfox[1]
  E_MSY <- -(par_Efox[1]/(par_Efox[2]*(1+(1/par_Efox[3]))))

  mE_fox <- seq(0,as.numeric(graph_param[1]),0.01)
  annee_etude <- as.numeric(table_Efox$Year[1]):as.numeric(table_Efox$Year[nrow(table_Efox)])

  IA_Efox <- (par_Efox[1]+par_Efox[2]*mE_fox)^(1/par_Efox[3])
  #IA_Efox <- par_Efox[1]*exp(-par_Efox[2]*mE_fox)
  #log_IA_Efox <- log(par_Efox[1]) - par_Efox[2]*mE_fox

  plotFOX_log <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="black")
  plotFOX_log <- plotFOX_log + geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black") + geom_path()
  plotFOX_log <- plotFOX_log + ggtitle(paste(graph_param[2]))
  plotFOX_log <- plotFOX_log + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  print(plotFOX_log)

  plotFOX_log_test <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="red")
  plotFOX_log_test <- plotFOX_log_test + ggtitle(paste(graph_param[2]))
  plotFOX_log_test <- plotFOX_log_test + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  plotFOX_log_test <- plotFOX_log_test + geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black") + geom_path(aes(x=table_Efox$Efox, y=table_Efox$IA), linetype="twodash")
  plotFOX_log_test <- plotFOX_log_test + geom_text(aes(x=table_Efox$Efox, y=table_Efox$IA, label=table_Efox$Year), hjust=0, vjust=0, size=2.5)
  print(plotFOX_log_test)


  Y_Efox <- IA_Efox * mE_fox *table_Efox$factEfox[1]
  #IA ~ (a+b*Efox)^(1/p)
  table_Efox$IA_pred <- (par_Efox[1]+par_Efox[2]*table_Efox$Efox)^(1/par_Efox[3])
  table_Efox2 <- table_Efox


  plotFOX2 <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="black")
  plotFOX2 <- plotFOX2 + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotFOX2 <- plotFOX2 + ggtitle(paste(graph_param[2]))
  plotFOX2 <- plotFOX2 + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  print(plotFOX2)

  plotFOX2_test <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="red")
  plotFOX2_test <- plotFOX2_test + ggtitle(paste(graph_param[2]))
  plotFOX2_test <- plotFOX2_test + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  plotFOX2_test <- plotFOX2_test + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black") + geom_path(aes(x=table_Efox$E, y=table_Efox$Capture), linetype="twodash") #E ou Efox? D.a dit de prendre E je crois
  plotFOX2_test <- plotFOX2_test + geom_text(aes(x=table_Efox$E, y=table_Efox$Capture, label=table_Efox$Year), hjust=0, vjust=0, size=2.5)
  print(plotFOX2_test)



  plotFOX3 <- ggplot() + geom_line(aes(x=table_Efox$Year, y=table_Efox$IA_pred), color="black")
  plotFOX3 <- plotFOX3 + geom_point(aes(x=table_Efox$Year, y=table_Efox$IA), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotFOX3 <- plotFOX3 + ggtitle(paste(graph_param[2]))
  plotFOX3 <- plotFOX3 + xlim(table_Efox$Year[1], table_Efox$Year[nrow(table_Efox)]) + ylim(0,as.numeric(graph_param[4]))
  print(plotFOX3)



  names_values <- c("Coeff_a", "Coeff_b", "Coeff_p", "MSY_recalcule", "Emsy_recalcule")
  results <- c(par_Efox[1], par_Efox[2], par_Efox[3], C_MSY, E_MSY)
  table_outputs <- rbind(names_values, results)

  print(table_outputs)

  return(table_outputs)

}

