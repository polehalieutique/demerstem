#' Generalised production model from IA and Efox series
#'
#' \code{generalised_model}
#'
#' @param table_Efox      : table with IA and Efox
#' @param graph_param     : vector gathering the graphhics aestetics parameters (format : c(lengthEfox, title, upper_x, upper_y, upper_ybis))
#' @param log             : by default, fit the model under log transformation of the AI. Else, fit without the log transformation (no start/limit)
#' @param a               : values of initialisation and range of the parameter a (lower, init, upper)
#' @param b               : values of initialisation and range of the parameter b
#' @param m               : values of initialisation and range of the parameter m
#' @param warning_control : if TRUE, the nls won't stop its process if too many warnings are made.
#' @param max_iteration   : number of iteration that will be runned.
#'
#' @examples
#'
#' @export


generalised_model <- function (table_Efox, graph_param, a = c(0.0001, 0.001, 0.01),
                               b = c(0.0001, 0.001, 0.01), m = c(0.5, 0.7, 0.9),
                               warning_control = FALSE, max_iteration = 50, IC = T)
{
  list_graph <- NULL
  if (warning_control == TRUE) {
    modelegene_IA <- nls(formula = log(IA) ~ log((a + b * Efox)^(1/(m-1))),
                         data = table_Efox, start = c(a = a[2], b = b[2],
                                                      m = m[2]), lower = c(a = a[1],
                                                                           b = b[1], m = m[1]), upper = c(a = a[3],
                                                                                                          b = b [3], m = m[3]), algorithm = "port",
                         nls.control(warnOnly = TRUE, maxiter = max_iteration))
  }
  else {
    modelegene_IA <- nls(formula = log(IA) ~ log((a + b * Efox)^(1/(m-1))),
                         data = table_Efox, start = c(a = a[2], b = b[2],
                                                      m = m[2]), lower = c(a = a[1],
                                                                           b = b[1], m = m[2]), upper = c(a = a[3],
                                                                                                          b = b[3], m = m[3]), algorithm = "port",
                         nls.control(maxiter = max_iteration))
  }
  #if (IC ==T ){
    x <- seq(0, graph_param[1], length = length(table_Efox$Efox))
    interval_confidence <- predictNLS(modelegene_IA, newdata = data.frame(Efox = x), interval = "confidence", alpha = 0.05, nsim = 10000)$summary %>% mutate(Efox = x)
    interval_confidence_pred <- predictNLS(modelegene_IA, newdata = data.frame(Efox = table_Efox$Efox[2:length(table_Efox$Efox)]), interval = "confidence", alpha = 0.05, nsim = 10000)$summary %>% mutate(Year = table_Efox$Year[2:length(table_Efox$Efox)])

    #if (log ==T) {
      interval_confidence$`Sim.2.5%` <- exp(interval_confidence$`Sim.2.5%`)
      interval_confidence$`Sim.97.5%` <- exp(interval_confidence$`Sim.97.5%`)

      interval_confidence_pred$`Sim.2.5%` <- exp(interval_confidence_pred$`Sim.2.5%`)
      interval_confidence_pred$`Sim.97.5%` <- exp(interval_confidence_pred$`Sim.97.5%`)
   # }
    interval_confidence$prod_low <- interval_confidence$`Sim.2.5%` * x *table_Efox$factEfox[1]
    interval_confidence$prod_up <- interval_confidence$`Sim.97.5%` * x *table_Efox$factEfox[1]
    #}

  par_Efox <- as.vector(coef(modelegene_IA))
  C_MSY <- -((par_Efox[3]-1)/par_Efox[2]) * ((par_Efox[1]/(1 +
                                                             (par_Efox[3]-1)))^(1 + (1/(par_Efox[3]-1)))) * table_Efox$factEfox[1]
  E_MSY <- -(par_Efox[1]/(par_Efox[2] * (1 + (1/(par_Efox[3]-1)))))
  mE_fox <- seq(0, as.numeric(graph_param[1]), 0.01)
  annee_etude <- as.numeric(table_Efox$Year[1]):as.numeric(table_Efox$Year[nrow(table_Efox)])
  IA_Efox <- (par_Efox[1] + par_Efox[2] * mE_fox)^(1/(par_Efox[3]-1))
  IA_MSY <- (par_Efox[1] + par_Efox[2] * E_MSY)^(1/(par_Efox[3]-1))
  Y_Efox <- IA_Efox * mE_fox * table_Efox$factEfox[1]


  table_Efox$IA_pred <- c(NA, exp(predict(modelegene_IA))) # to plot IA_pred
  table_Efox2 <- table_Efox
  data <- data.frame(mE_fox, IA_Efox, Y_Efox)
  todo <- data.frame(title = c(rep(graph_param[2], 4), paste0(graph_param[2],
                                                              "\n Abundance indices predicted vs observed")))
  plotGEN_U <- table_Efox %>% mutate(title = todo[length(list_graph) +
                                                    1, ]) %>% ggplot() + geom_point(aes(x = Efox, y = IA),
                                                                                    color = "black", size = 1) + geom_path(aes(x = Efox,
                                                                                                                               y = IA)) + facet_grid(~title)  + geom_hline(yintercept = rep(table_Efox$IA[nrow(table_Efox)],
                                                                                                                                                                                            nrow(table_Efox)), linetype = "dashed", color = "red") +
    geom_vline(xintercept = rep(1, nrow(table_Efox)), linetype = "dashed",
               color = "red") + theme_nice() + labs(x = "mE", y = "Abundance indices") +
    geom_line(data = data, aes(x = mE_fox, y = IA_Efox),
              color = "blue", size = 1.1)
  if (IC ==T) {plotGEN_U <- plotGEN_U + geom_ribbon(data = interval_confidence,
                                                    aes(x = Efox, ymin = `Sim.2.5%`, ymax = `Sim.97.5%`),
                                                    alpha = 0.15, inherit.aes = F, fill = "blue")}
  list_graph[[length(list_graph) + 1]] <- plotGEN_U
  plotGEN_U_2 <- table_Efox %>% mutate(title = todo[length(list_graph) +
                                                      1, ]) %>% ggplot() + geom_point(aes(x = Efox, y = IA),
                                                                                      color = "black", size = 1) + facet_grid(~title) + geom_line(data = data,
                                                                                                                                                  aes(x = mE_fox, y = IA_Efox), color = "blue", size = 1.1) +
    geom_hline(yintercept = rep(table_Efox$IA[nrow(table_Efox)],
                                nrow(table_Efox)), linetype = "dashed", color = "red") +
    geom_vline(xintercept = rep(1, nrow(table_Efox)), linetype = "dashed",
               color = "red") + geom_path(aes(x = Efox, y = IA),
                                          linetype = "twodash", size = 1) + geom_text(aes(x = table_Efox$Efox,
                                                                                          y = table_Efox$IA, label = stringi::stri_sub(table_Efox$Year,
                                                                                                                                       3, 4)), hjust = -0.5, vjust = 0, size = 4) + theme_nice() +
    labs(x = "mE", y = "Abundance indices")
  if (IC ==T) {plotGEN_U_2 <- plotGEN_U_2 + geom_ribbon(data = interval_confidence,
                                                        aes(x = Efox, ymin = `Sim.2.5%`, ymax = `Sim.97.5%`),
                                                        alpha = 0.15, inherit.aes = F, fill = "blue")}

  list_graph[[length(list_graph) + 1]] <- plotGEN_U_2
  plotGEN_Y <- table_Efox %>% mutate(title = todo[length(list_graph) +
                                                    1, ]) %>% ggplot() + geom_point(aes(x = E, y = Capture),
                                                                                    color = "black", size = 1.3) + facet_grid(~title) +
    labs(x = "mE") + geom_hline(yintercept = rep(table_Efox$Capture[nrow(table_Efox)],
                                                 nrow(table_Efox)), linetype = "dashed", color = "red") +
    geom_vline(xintercept = rep(table_Efox$E[nrow(table_Efox)],
                                nrow(table_Efox)), linetype = "dashed", color = "red") +
    theme_nice() + labs(x = "mE", y = "Catch") + geom_line(data = data,
                                                           aes(x = mE_fox, y = Y_Efox), color = "blue", size = 1.1)
  if (IC ==T) {plotGEN_Y <- plotGEN_Y + geom_ribbon(data = interval_confidence,
                                                    aes(x = Efox, ymin = prod_low, ymax = prod_up), alpha = 0.15,
                                                    inherit.aes = F, fill = "blue")}

  list_graph[[length(list_graph) + 1]] <- plotGEN_Y
  plotGEN_Y_2 <- table_Efox %>% mutate(title = todo[length(list_graph) +
                                                      1, ]) %>% ggplot() + geom_point(aes(x = E, y = Capture),
                                                                                      color = "black", size = 1) + facet_grid(~title) + geom_path(aes(x = E,
                                                                                                                                                      y = Capture), linetype = "twodash", size = 1.1) + geom_text(aes(x = E,
                                                                                                                                                                                                                      y = Capture, label = stringi::stri_sub(Year, 3, 4)),
                                                                                                                                                                                                                  hjust = -0.5, vjust = 0, size = 4) + geom_hline(yintercept = rep(table_Efox$Capture[nrow(table_Efox)],
                                                                                                                                                                                                                                                                                   nrow(table_Efox)), linetype = "dashed", color = "red") +
    geom_vline(xintercept = rep(table_Efox$E[nrow(table_Efox)],
                                nrow(table_Efox)), linetype = "dashed", color = "red") +
    labs(x = "mE", y = "Catch") + theme_nice() + geom_line(data = data,
                                                           aes(x = mE_fox, y = Y_Efox), color = "blue", size = 1.1)

  if (IC ==T) {plotGEN_Y_2 <- plotGEN_Y_2 + geom_ribbon(data = interval_confidence,
                                                        aes(x = Efox, ymin = prod_low, ymax = prod_up), alpha = 0.15,
                                                        inherit.aes = F, fill = "blue")
  }


  list_graph[[length(list_graph) + 1]] <- plotGEN_Y_2


 # if (IC ==T) {
    plotGEN_pred <- table_Efox %>% full_join(interval_confidence_pred[,
                                                                      c(11:13)], by = "Year") %>% mutate(title = todo[length(list_graph) +
                                                                                                                        1, ]) %>% ggplot() + geom_line(aes(x = Year, y = IA_pred),
                                                                                                                                                       color = "black") + geom_point(aes(x = Year, y = IA),
                                                                                                                                                                                     color = "black") + geom_ribbon(aes(x = Year, ymin = `Sim.2.5%`,
                                                                                                                                                                                                                        ymax = `Sim.97.5%`), alpha = 0.15, inherit.aes = F,
                                                                                                                                                                                                                    fill = "#333232") + facet_grid(~title) + theme_nice() +
      labs(x = "Year", y = "Abundance indices")  + ylim(c(0,max(table_Efox$IA)))
    list_graph[[length(list_graph) + 1]] <- plotGEN_pred#}

  plot(nlsResiduals(modelegene_IA))


  names_values <- c("a", "b", "m",
                    "MSY",
                    "E/E_msy",
                    "B/BMSY",
                    "B/B0",
                    "AIC")

  results <- round(c(par_Efox[1], par_Efox[2], (par_Efox[3]),
                     C_MSY,
                     tail(table_Efox$Efox,1)/E_MSY,
                     tail(table_Efox$IA,1)/IA_MSY,
                     tail(table_Efox$IA,1)/IA_Efox[1],
                     AIC(modelegene_IA)), 5)

  table_pseudo_eq_gene <- rbind(names_values, results)
  print(table_pseudo_eq_gene)
  assign('table_pseudo_eq_gene', table_pseudo_eq_gene, envir=globalenv())
  return(list(table_pseudo_eq_gene, list_graph))
}
