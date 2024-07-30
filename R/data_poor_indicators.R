#'  Estimate some data poors indicators
#'
#' \code{data_poor_indicators}
#'
#' @param   Catch_IA          data with weight and size vectors
#' @param   L_inf             Asymptotic length
#' @param   year              Year of interest you want to study and for which will be calculated 'f'
#' @param   plot              if TRUE, plot all the years
#' @examples
#'
#'
#' @export

data_poor_indicators <- function(data_freq, Catch_IA = NA, L_inf, year = NA, plot = T) {

  plotlist <- list()

  if(is.na(year) ==T){
    year <-  max(data_freq$year)
  }

  if (is.na(Catch_IA) != T) {
    Catch_IA <- Catch_IA[order(Catch_IA$annee),]
  }

  data_indice <- data.frame()
  data_frequency <- data_freq
  for (k in sort(unique(data_frequency$year))) {
    data_freq <- data_frequency %>% filter(year==k) %>% group_by(lclass, year) %>% dplyr::summarise(frequence = sum(frequence, na.rm = T))

    Lmax <- data_freq$lclass[which.min(abs(data_freq$frequence[data_freq$frequence<=max(data_freq$frequence)] - max(data_freq$frequence))/2)]

    Lc <- data_freq$lclass[which.min(abs(data_freq$frequence[data_freq$lclass<=Lmax] - max(data_freq$frequence)/2))]

    LFM <- 0.25 * Lc + 0.75*L_inf

    Lmean <- weighted.mean(data_freq$lclass, data_freq$frequence)

    data_indice <- rbind(data_indice, data.frame(annee = k, Lc = Lc, Lmean = Lmean, LFM = LFM))

    data_freq <- data_frequency%>% filter(year==k) %>% group_by(lclass) %>% dplyr::summarise(frequence = sum(frequence, na.rm = T)) %>%  mutate(title = paste0(k," - Lc = ", Lc))

    if (k == year) {
      plot(ggplot(data = data_freq, aes(x = lclass/L_inf, y = frequence/sum(frequence))) + geom_line(color = 'red', size = 1.1) + geom_vline(xintercept = Lc/L_inf, linetype="dashed",
                                                                                                                                             color = "black", size=1) +xlim(0,1.5) + theme_nice() +labs(x = 'L / L_inf', y = "Relative frequency") + facet_grid(~title))
    }

    if (plot == T) {
      plotlist[[k-min(unique(data_frequency$year))+1]] <- ggplot(data = data_freq, aes(x = lclass/L_inf, y = frequence/sum(frequence))) + geom_line(color = 'red', size = 1.1) + geom_vline(xintercept = Lc/L_inf, linetype="dashed",
                                                                                                                                                                                            color = "black", size=1) +xlim(0,1.5) + theme_nice() +labs(x = 'L / L_inf', y = "Relative frequency") + facet_grid(~title)
    }
  }
  if (plot == T){
    plot(cowplot::plot_grid(plotlist = plotlist))
  }

  Fproxy<- NULL
  for (k in 2:(length(data_indice$Lmean-1))) {

    f <- data_indice$Lmean[k - 1]/data_indice$LFM[k]

    if (f >1) {
      Fproxy <- c(Fproxy, Catch_IA$catch[Catch_IA$annee == year]/Catch_IA$IA[Catch_IA$annee == year])
    }
  }
  if (is.null(Fproxy)) {
    print(paste0("Fproxy = non calculable"))
  }
  else{
    Fproxy <- mean(Fproxy)
    print(paste0("Fproxy = ", Fproxy))
  }
  if (is.na(Catch_IA) != T) {
    r = mean(Catch_IA$IA[(dim(Catch_IA)[1]-1):(dim(Catch_IA)[1])], na.rm = T)/                  mean(Catch_IA$IA[(dim(Catch_IA)[1]-5):(dim(Catch_IA)[1]-3)], na.rm =T)
    print(paste0("r = ", round(r, digits = 3)))
    b = min(1, Catch_IA$IA[dim(Catch_IA)[1]]/(1.4*min(Catch_IA$IA, na.rm = T)))
    print(paste0("b = ", round(b, digits = 3)))
  }
  f = data_indice$Lmean[which.min(abs(year - data_indice$annee))-1]/data_indice$LFM[data_indice$annee == year]
  print(paste0("f = ", round(f, digits = 3)))
  return(plotlist)
}
  #data_indice <- full_join(data_indice,IA , by = 'annee')
  # print(f)
  # if(k < 0.2) {
  #   print('method 2.1 : rfmb rule')
  #   m = 0.95
  #   C = C * m * r * f * b
  #   print(C)
  # }
  # if (0.2 <= k < 0.32) {
  #   print('method 2.1 : rfmb rule')
  #   m = 0.9
  #   C = C * m * r * f * b
  #   print(C)
  # }
  # if (0.32 <= k < 0.45) {
  #   print('method 2.2 : chr rule')
  #   r = IA[(length(IA)-1)]
  #   m = 0.5
  #   b = IA[length(IA)]/(1.4*min(IA))
  #   C = IA[length(IA)-1] * (C/IA[length(IA)]) * m * b
  #   print(C)
  # }
  # if (k >= 0.45) {
  #   r = mean(IA[length(IA)])/mean(IA[(length(IA)-2):(length(IA)-1)])
  #   b = IA[length(IA)]/(1.4*min(IA))
  #   C = C * r * b
  #   }
  # }
  # else {
  #
  # }

