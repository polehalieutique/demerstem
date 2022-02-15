## brouillon

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


generalised_model <- function(table_Efox, graph_param, vect_ini=c(10, -0.5, 1), vect_lower=c(0.5, -1, 0.01), vect_upper=c(100, 10, 9), warning_control = FALSE, max_iteration = 50){


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
  Y_Efox <- IA_Efox * mE_fox *table_Efox$factEfox[1]
  #IA ~ (a+b*Efox)^(1/p)
  table_Efox$IA_pred <- (par_Efox[1]+par_Efox[2]*table_Efox$Efox)^(1/par_Efox[3])
  table_Efox2 <- table_Efox
  #IA_Efox <- par_Efox[1]*exp(-par_Efox[2]*mE_fox)
  #log_IA_Efox <- log(par_Efox[1]) - par_Efox[2]*mE_fox


  table_Efox$title <- graph_param[2]
  plotFOX_log <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="blue", size = 1.1)
  #plotFOX_log <- plotFOX_log + ggtitle(paste(graph_param[2]))
  plotFOX_log <- plotFOX_log +
    geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black", size = 1) +
    geom_path() +
    facet_grid(~table_Efox$title) +
    geom_hline(yintercept= rep(table_Efox$IA[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +geom_vline(xintercept= rep(1,nrow(table_Efox)), linetype="dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Abundance indices")
  #plotFOX_log <- plotFOX_log + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  print(plotFOX_log)



  plotFOX_log_test <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="blue", size = 1.1)
  plotFOX_log_test <- plotFOX_log_test +
    geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black", size = 1) +
    geom_hline(yintercept= rep(table_Efox$IA[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +geom_vline(xintercept= rep(1,nrow(table_Efox)), linetype="dashed", color = "red") +
    geom_path(aes(x=table_Efox$Efox, y=table_Efox$IA), linetype="twodash", size = 1)
  plotFOX_log_test <- plotFOX_log_test +
    geom_text(aes(x=table_Efox$Efox, y=table_Efox$IA, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=4) +    facet_grid(~table_Efox$title) +
    theme_nice() + labs(x = "mE", y = "Abundance indices")
  #plotFOX_log_test <- plotFOX_log_test + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))

  print(plotFOX_log_test)



  plotFOX2 <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="blue", size = 1.1)
  plotFOX2 <- plotFOX2 + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black", size = 1.3)+ facet_grid(~table_Efox$title) + labs(x = "mE") +
    geom_hline(yintercept= rep(table_Efox$Capture[nrow(table_Efox)], nrow(table_Efox)), linetype="dashed", color = "red") + geom_vline(xintercept= rep(table_Efox$E[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Catch") # D.a dit de prendre E
  #plotFOX2 <- plotFOX2 + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  print(plotFOX2)

  plotFOX2_test <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="blue", size = 1)
  #plotFOX2_test <- plotFOX2_test + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  plotFOX2_test <- plotFOX2_test + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black", size = 1) + geom_path(aes(x=table_Efox$E, y=table_Efox$Capture), linetype="twodash", size = 1.1) #E ou Efox? D.a dit de prendre E je crois
  plotFOX2_test <- plotFOX2_test + geom_text(aes(x=table_Efox$E, y=table_Efox$Capture, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=4) +
    geom_hline(yintercept= rep(table_Efox$Capture[nrow(table_Efox)], nrow(table_Efox)), linetype="dashed", color = "red") + geom_vline(xintercept= rep(table_Efox$E[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +
    facet_grid(~table_Efox$title) + labs(x = "mE", y = "Catch") + theme_nice()
  print(plotFOX2_test)


  table_Efox$title <- paste0(table_Efox$title, "\n Abundance indices predicted vs observed")
  plotFOX3 <- ggplot() + geom_line(aes(x=table_Efox$Year, y=table_Efox$IA_pred), color="black")
  plotFOX3 <- plotFOX3 + geom_point(aes(x=table_Efox$Year, y=table_Efox$IA), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotFOX3 <- plotFOX3  + facet_grid(~table_Efox$title) + theme_nice() + labs(x = "Year", y = "Abundance indices" )
  #plotFOX3 <- plotFOX3 + xlim(table_Efox$Year[1], table_Efox$Year[nrow(table_Efox)]) + ylim(0,as.numeric(graph_param[4]))
  print(plotFOX3)



  names_values <- c("Coeff_a", "Coeff_b", "Coeff_p", "MSY_recalcule", "Emsy_recalcule")
  results <- c(par_Efox[1], par_Efox[2], par_Efox[3], C_MSY, E_MSY)
  table_outputs <- rbind(names_values, results)

  print(table_outputs)

  return(table_outputs)

}

