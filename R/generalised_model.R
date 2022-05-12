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

  list_graph <- NULL
  if (warning_control==TRUE){
    modelegene_IA <- nls(formula = IA ~ (a+b*Efox)^(1/p), data=table_Efox, start = c(a = vect_ini[1] , b = vect_ini[2], p=vect_ini[3]), lower = c(a = vect_lower[1] , b=vect_lower[2], p = vect_lower[3]), upper = c(a = vect_upper[1], b=vect_upper[2], p = vect_upper[3]), algorithm = "port", nls.control(warnOnly = TRUE, maxiter = max_iteration))
  } else {
    modelegene_IA <- nls(formula = IA ~ (a+b*Efox)^(1/p), data=table_Efox, start = c(a = vect_ini[1] , b = vect_ini[2], p=vect_ini[3]), lower = c(a = vect_lower[1] , b=vect_lower[2], p = vect_lower[3]), upper = c(a = vect_upper[1], b=vect_upper[2], p = vect_upper[3]), algorithm = "port", nls.control(maxiter = max_iteration))
  }

  x <- seq(0, graph_param[1], length = length(table_Efox$Efox))
  interval_confidence <- predictNLS(modelegene_IA, newdata=data.frame(Efox = x), interval="confidence", alpha=0.05, nsim=10000)$summary %>% mutate(Efox = x)

  par_Efox <- as.vector(coef(modelegene_IA))
  C_MSY <- -(par_Efox[3]/par_Efox[2])*((par_Efox[1]/(1+par_Efox[3]))^(1+(1/par_Efox[3])))*table_Efox$factEfox[1]
  E_MSY <- -(par_Efox[1]/(par_Efox[2]*(1+(1/par_Efox[3]))))

  mE_fox <- seq(0,as.numeric(graph_param[1]),0.01)
  annee_etude <- as.numeric(table_Efox$Year[1]):as.numeric(table_Efox$Year[nrow(table_Efox)])

  IA_Efox <- (par_Efox[1]+par_Efox[2]*mE_fox)^(1/par_Efox[3])
  Y_Efox <- IA_Efox * mE_fox *table_Efox$factEfox[1]
  #IA ~ (a+b*Efox)^(1/p)
  interval_confidence$prod_low <- interval_confidence$`Sim.2.5%` * x *table_Efox$factEfox[1]
  interval_confidence$prod_up <- interval_confidence$`Sim.97.5%` * x *table_Efox$factEfox[1]
  # NA for first value because no E_fox obs
  table_Efox$IA_pred <- c(NA, predict(modelegene_IA)) # (par_Efox[1]+par_Efox[2]*table_Efox$Efox)^(1/par_Efox[3])


  table_Efox2 <- table_Efox
  #IA_Efox <- par_Efox[1]*exp(-par_Efox[2]*mE_fox)
  #log_IA_Efox <- log(par_Efox[1]) - par_Efox[2]*mE_fox


  table_Efox$title <- graph_param[2]
  plotGEN_U <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="blue", size = 1.1)
  #plotGEN_U <- plotGEN_U + ggtitle(paste(graph_param[2]))
  plotGEN_U <- plotGEN_U +
    geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black", size = 1) +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=`Sim.2.5%`, ymax= `Sim.97.5%`), alpha = 0.15, inherit.aes=F, fill="blue") +
    geom_path() +
    facet_grid(~table_Efox$title) +
    geom_hline(yintercept= rep(table_Efox$IA[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +geom_vline(xintercept= rep(1,nrow(table_Efox)), linetype="dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Abundance indices")
  #plotGEN_U <- plotGEN_U + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))
  list_graph[[length(list_graph) + 1]] <- list(plotGEN_U)



  plotGEN_U_2 <- ggplot() + geom_line(aes(x=mE_fox, y=IA_Efox), color="blue", size = 1.1)
  plotGEN_U_2 <- plotGEN_U_2 +
    geom_point(aes(x=table_Efox$Efox, y=table_Efox$IA), color="black", size = 1) +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=`Sim.2.5%`, ymax= `Sim.97.5%`), alpha = 0.2, inherit.aes=F, fill="blue") +
    geom_hline(yintercept= rep(table_Efox$IA[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +geom_vline(xintercept= rep(1,nrow(table_Efox)), linetype="dashed", color = "red") +
    geom_path(aes(x=table_Efox$Efox, y=table_Efox$IA), linetype="twodash", size = 1)
  plotGEN_U_2 <- plotGEN_U_2 +
    geom_text(aes(x=table_Efox$Efox, y=table_Efox$IA, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=4) +    facet_grid(~table_Efox$title) +
    theme_nice() + labs(x = "mE", y = "Abundance indices")
  #plotGEN_U_2 <- plotGEN_U_2 + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[4]))

  list_graph[[length(list_graph) + 1]] <- list(plotGEN_log_2)



  plotGEN_Y <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="blue", size = 1.1) +
    geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=prod_low, ymax= prod_up), alpha=0.15, inherit.aes=F, fill="blue")
  plotGEN_Y <- plotGEN_Y + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black", size = 1.3)+ facet_grid(~table_Efox$title) + labs(x = "mE") +
    geom_hline(yintercept= rep(table_Efox$Capture[nrow(table_Efox)], nrow(table_Efox)), linetype="dashed", color = "red") + geom_vline(xintercept= rep(table_Efox$E[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Catch") # D.a dit de prendre E
  #plotGEN_Y <- plotGEN_Y + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  list_graph[[length(list_graph) + 1]] <- list(plotGEN_Y)

  plotGEN_Y_2 <- ggplot() + geom_line(aes(x=mE_fox, y=Y_Efox), color="blue", size = 1) +
  geom_ribbon(data=interval_confidence, aes(x=Efox, ymin=prod_low, ymax= prod_up), alpha = 0.2, inherit.aes=F, fill="blue")
  #plotGEN_Y_2 <- plotGEN_Y_2 + xlim(0,as.numeric(graph_param[3])) + ylim(0,as.numeric(graph_param[5]))
  plotGEN_Y_2 <- plotGEN_Y_2 + geom_point(aes(x=table_Efox$E, y=table_Efox$Capture), color="black", size = 1) + geom_path(aes(x=table_Efox$E, y=table_Efox$Capture), linetype="twodash", size = 1.1) #E ou Efox? D.a dit de prendre E je crois
  plotGEN_Y_2 <- plotGEN_Y_2 + geom_text(aes(x=table_Efox$E, y=table_Efox$Capture, label=stringi::stri_sub(table_Efox$Year,3,4)), hjust=-0.5, vjust=0, size=4) +
    geom_hline(yintercept= rep(table_Efox$Capture[nrow(table_Efox)], nrow(table_Efox)), linetype="dashed", color = "red") + geom_vline(xintercept= rep(table_Efox$E[nrow(table_Efox)],nrow(table_Efox)), linetype="dashed", color = "red") +
    facet_grid(~table_Efox$title) + labs(x = "mE", y = "Catch") + theme_nice()
  list_graph[[length(list_graph) + 1]] <- list(plotGEN_Y_2)


  table_Efox$title <- paste0(table_Efox$title, "\n Abundance indices predicted vs observed")
  plotGEN_pred <- ggplot() + geom_line(aes(x=table_Efox$Year, y=table_Efox$IA_pred), color="black")
  plotGEN_pred <- plotGEN_pred + geom_point(aes(x=table_Efox$Year, y=table_Efox$IA), color="black") #E ou Efox? D.a dit de prendre E je crois
  plotGEN_pred <- plotGEN_pred  + facet_grid(~table_Efox$title) + theme_nice() + labs(x = "Year", y = "Abundance indices" )
  #plotGEN_pred <- plotGEN_pred + xlim(table_Efox$Year[1], table_Efox$Year[nrow(table_Efox)]) + ylim(0,as.numeric(graph_param[4]))
  list_graph[[length(list_graph) + 1]] <- list(plotGEN_pred)



  names_values <- c("Coeff_a", "Coeff_b", "Coeff_p", "MSY_recalcule", "Emsy_recalcule", "AIC")
  results <- round(c(par_Efox[1], par_Efox[2], par_Efox[3], C_MSY, E_MSY, AIC(modelegene_IA)),2)
  table_outputs <- rbind(names_values, results)

  print(table_outputs)

  return(list(table_outputs, list_graph))

}

